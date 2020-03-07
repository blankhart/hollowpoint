{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.CodeGen.Dart.Make.Actions
  ( backendMakeActions
  ) where

import Debug.Trace

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Aeson.Casing (snakeCase)
import           Data.Foldable (for_, minimum, foldl')
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import           Language.PureScript.AST
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreFn.ToJSON as CFJ
import           Language.PureScript.Crash
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile)
import qualified Language.PureScript.Make.Actions as Default
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names (runModuleName, ModuleName)
import           Paths_hollowpoint as Paths
-- import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))

import Language.PureScript.CodeGen.Dart.Make.Foreigns as Dart
import Language.PureScript.CodeGen.Dart.Make.Monad as Dart
import Language.PureScript.CodeGen.Dart.Options as Dart hiding (codegenTargets)
import Language.PureScript.CodeGen.Dart.Printer as Dart
import Language.PureScript.CodeGen.Dart.CoreImp as Dart

-- | Render a progress message
renderProgressMessage :: Default.ProgressMessage -> String
renderProgressMessage (Default.CompilingModule mn) =
  "Compiling " ++ T.unpack (runModuleName mn)

-- | A set of make actions that read and write modules from the given directory.
backendMakeActions
  :: FilePath
  -- ^ the base output directory
  -> M.Map ModuleName (Either Default.RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign Dart for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> Default.MakeActions Dart.Make
backendMakeActions outputDir filePathMap foreigns usePrefix =
    Default.MakeActions getInputTimestampsAndHashes getOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb outputPrimDocs
  where

  getInputTimestampsAndHashes
    :: ModuleName
    -> Dart.Make (Either Default.RebuildPolicy (M.Map FilePath (UTCTime, Dart.Make ContentHash)))
  getInputTimestampsAndHashes mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    case path of
      Left policy ->
        return (Left policy)
      Right filePath -> do
        let inputPaths = filePath : maybeToList (M.lookup mn foreigns)
            getInfo fp = do
              ts <- Dart.getTimestamp fp
              return (ts, Dart.hashFile fp)
        pathsWithInfo <- traverse (\fp -> (fp,) <$> getInfo fp) inputPaths
        return $ Right $ M.fromList pathsWithInfo

  -- TODO: This will need to be coordinated with printing due to imports.
  -- Don't depend on another snakeCase - do own conversions.
  -- The conversion should handle acronyms responsibly, e.g.:
  --  * WebHTML -> web_html
  --  * XMLSchema -> xml_schema
  --  * DoMTVWatching -> do_mtv_watching
  --  Rule: Initial capital is lowered, but successive capitals are sustained
  --  except the last before the next lowering.
  --
  -- TODO: The bundler would need to call the "main" function from the
  -- relevant library in the case of command line apps.
  -- But this relates to how pub manages packages.  Where would it look?
  -- And how will PS packages be distributed?  If as source, the pub-like tool
  -- would need to refer to local directory for import 'package:purescript/...'
  -- syntax.
  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let
      segments =
        map snakeCase .
        map T.unpack .
        T.split (=='.') $
          runModuleName mn
      modulePath = foldl' (</>) "" segments
    in
      outputDir </> "lib" </> modulePath </> fn

  targetFilename :: ModuleName -> Dart.CodegenTarget -> FilePath
  targetFilename mn = \case
    Dart.Dart -> outputFilename mn "index.dart"
    Dart.CoreFn -> outputFilename mn "corefn.json"
    Dart.Docs -> outputFilename mn "docs.json"

  getOutputTimestamp :: ModuleName -> Dart.Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    let outputPaths = [outputFilename mn "externs.json"] <> fmap (targetFilename mn) (S.toList codegenTargets)
    timestamps <- traverse getTimestampMaybe outputPaths
    pure $ fmap minimum . NEL.nonEmpty =<< sequence timestamps

  readExterns :: ModuleName -> Dart.Make (FilePath, Maybe ExternsFile)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> "externs.json"
    (path, ) <$> readExternsFile path

  outputPrimDocs :: Dart.Make ()
  outputPrimDocs = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member Docs codegenTargets) $ for_ Docs.Prim.primModules $ \docsMod@Docs.Module{..} ->
      writeJSONFile (outputFilename modName "docs.json") docsMod

  codegen :: CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT Dart.Make ()
  codegen m docs exts = do
    let mn = CF.moduleName m
    lift $ writeJSONFile (outputFilename mn "externs.json") exts
    codegenTargets <- lift $ asks optionsCodegenTargets
    when (S.member CoreFn codegenTargets) $ do
      let coreFnFile = targetFilename mn CoreFn
          json = CFJ.moduleToJSON version m
      lift $ writeJSONFile coreFnFile json
    when (S.member Dart codegenTargets) $ do
      foreignInclude <- case mn `M.lookup` foreigns of
        Just _
          | not $ requiresForeign m -> return Nothing
          | otherwise -> return $ Just $ "./foreign.dart"
        Nothing
          | requiresForeign m ->
              throwError . errorMessage' (CF.moduleSourceSpan m) $
                MissingFFIModule mn
          | otherwise -> return Nothing
      rawJs <- Dart.moduleToJs m foreignInclude
      let pjs = Dart.prettyPrintJS rawJs
          jsFile = targetFilename mn Dart
          prefix =
            ["Generated by hollowpoint version " <> T.pack (showVersion Paths.version)
            | usePrefix]
          js = T.unlines $ map ("// " <>) prefix ++ [pjs]
      lift $ writeTextFile jsFile (TE.encodeUtf8 js)

    when (S.member Docs codegenTargets) $ do
      lift $ writeJSONFile (outputFilename mn "docs.json") docs

  ffiCodegen :: CF.Module CF.Ann -> Dart.Make ()
  ffiCodegen m = do
    codegenTargets <- asks Dart.optionsCodegenTargets
    when (S.member Dart codegenTargets) $ do
      let mn = CF.moduleName m
      case mn `M.lookup` foreigns of
        Just path
          | not $ requiresForeign m ->
              tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
          | otherwise ->
              Dart.checkForeignDecls m path
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return ()
      for_ (mn `M.lookup` foreigns) $ \path ->
        Dart.copyFile path (outputFilename mn "foreign.dart")

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  progress :: Default.ProgressMessage -> Dart.Make ()
  progress = liftIO . putStrLn . renderProgressMessage

  readCacheDb :: Dart.Make CacheDb
  readCacheDb = fmap (fromMaybe mempty) $ Dart.readJSONFile cacheDbFile

  writeCacheDb :: CacheDb -> Dart.Make ()
  writeCacheDb = Dart.writeJSONFile cacheDbFile

  cacheDbFile = outputDir </> "cache-db.json"

