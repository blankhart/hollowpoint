{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.CodeGen.Dart.Make.Actions
  ( backendMakeActions
  ) where

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Foldable (for_, minimum)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime)
import           Data.Version (makeVersion, showVersion)
import           Language.PureScript.AST
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreFn.ToJSON as CFJ
import           Language.PureScript.Crash
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile)
import           Language.PureScript.Make.Actions
import           Language.PureScript.Make.Monad
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names (runModuleName, ModuleName)
import           Language.PureScript.Options hiding (codegenTargets)
import           Language.PureScript.Pretty.Common (SMap(..))
import           SourceMap
import           SourceMap.Types
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>), makeRelative, splitPath, normalise)

import           Language.PureScript.CodeGen.Dart.Printer
import qualified Language.PureScript.CodeGen.Dart.CoreImp as Dart
import qualified Language.PureScript.CodeGen.Dart.CoreImp.AST as Imp

-- FIXME: checkForeignDecls for Dart

-- | Render a progress message
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) =
  "Compiling " ++ T.unpack (runModuleName mn)

-- | A set of make actions that read and write modules from the given directory.
backendMakeActions
  :: FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> MakeActions Make
backendMakeActions outputDir filePathMap foreigns usePrefix =
    MakeActions getInputTimestampsAndHashes getOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb outputPrimDocs
  where

  getInputTimestampsAndHashes
    :: ModuleName
    -> Make (Either RebuildPolicy (M.Map FilePath (UTCTime, Make ContentHash)))
  getInputTimestampsAndHashes mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    case path of
      Left policy ->
        return (Left policy)
      Right filePath -> do
        let inputPaths = filePath : maybeToList (M.lookup mn foreigns)
            getInfo fp = do
              ts <- getTimestamp fp
              return (ts, hashFile fp)
        pathsWithInfo <- traverse (\fp -> (fp,) <$> getInfo fp) inputPaths
        return $ Right $ M.fromList pathsWithInfo

  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let filePath = T.unpack (runModuleName mn)
    in outputDir </> filePath </> fn

  targetFilename :: ModuleName -> CodegenTarget -> FilePath
  targetFilename mn = \case
    JS -> outputFilename mn "index.js"
    JSSourceMap -> outputFilename mn "index.js.map"
    CoreFn -> outputFilename mn "corefn.json"
    Docs -> outputFilename mn "docs.json"

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    let outputPaths = [outputFilename mn "externs.json"] <> fmap (targetFilename mn) (S.toList codegenTargets)
    timestamps <- traverse getTimestampMaybe outputPaths
    pure $ fmap minimum . NEL.nonEmpty =<< sequence timestamps

  readExterns :: ModuleName -> Make (FilePath, Maybe ExternsFile)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> "externs.json"
    (path, ) <$> readExternsFile path

  outputPrimDocs :: Make ()
  outputPrimDocs = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member Docs codegenTargets) $ for_ Docs.Prim.primModules $ \docsMod@Docs.Module{..} ->
      writeJSONFile (outputFilename modName "docs.json") docsMod

  codegen :: CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT Make ()
  codegen m docs exts = do
    let mn = CF.moduleName m
    lift $ writeJSONFile (outputFilename mn "externs.json") exts
    codegenTargets <- lift $ asks optionsCodegenTargets
    when (S.member CoreFn codegenTargets) $ do
      let coreFnFile = targetFilename mn CoreFn
          json = CFJ.moduleToJSON version m
      lift $ writeJSONFile coreFnFile json
    when (S.member JS codegenTargets) $ do
      foreignInclude <- case mn `M.lookup` foreigns of
        Just _
          | not $ requiresForeign m -> do
              return Nothing
          | otherwise -> do
              return $ Just $ Imp.App Nothing (Imp.Var Nothing "require") [Imp.StringLiteral Nothing "./foreign.js"]
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return Nothing
      rawJs <- Dart.moduleToJs m foreignInclude
      dir <- lift $ makeIO "get the current directory" getCurrentDirectory
      let sourceMaps = S.member JSSourceMap codegenTargets
          (pjs, mappings) = if sourceMaps then prettyPrintJSWithSourceMaps rawJs else (prettyPrintJS rawJs, [])
          jsFile = targetFilename mn JS
          mapFile = targetFilename mn JSSourceMap
          prefix = ["Generated by purs version " <> T.pack (showVersion version) | usePrefix]
          js = T.unlines $ map ("// " <>) prefix ++ [pjs]
          mapRef = if sourceMaps then "//# sourceMappingURL=index.js.map\n" else ""
      lift $ do
        writeTextFile jsFile (TE.encodeUtf8 $ js <> mapRef)
        when sourceMaps $ genSourceMap dir mapFile (length prefix) mappings
    when (S.member Docs codegenTargets) $ do
      lift $ writeJSONFile (outputFilename mn "docs.json") docs

  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen m = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member JS codegenTargets) $ do
      let mn = CF.moduleName m
      case mn `M.lookup` foreigns of
        Just path
          | not $ requiresForeign m ->
              tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
          | otherwise ->
              checkForeignDecls m path
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return ()
      for_ (mn `M.lookup` foreigns) $ \path ->
        copyFile path (outputFilename mn "foreign.js")

  genSourceMap :: String -> String -> Int -> [SMap] -> Make ()
  genSourceMap dir mapFile extraLines mappings = do
    let pathToDir = iterate (".." </>) ".." !! length (splitPath $ normalise outputDir)
        sourceFile = case mappings of
                      (SMap file _ _ : _) -> Just $ pathToDir </> makeRelative dir (T.unpack file)
                      _ -> Nothing
    let rawMapping = SourceMapping { smFile = "index.js", smSourceRoot = Nothing, smMappings =
      map (\(SMap _ orig gen) -> Mapping {
          mapOriginal = Just $ convertPos $ add 0 (-1) orig
        , mapSourceFile = sourceFile
        , mapGenerated = convertPos $ add (extraLines+1) 0 gen
        , mapName = Nothing
        }) mappings
    }
    let mapping = generate rawMapping
    writeJSONFile mapFile mapping
    where
    add :: Int -> Int -> SourcePos -> SourcePos
    add n m (SourcePos n' m') = SourcePos (n+n') (m+m')

    convertPos :: SourcePos -> Pos
    convertPos SourcePos { sourcePosLine = l, sourcePosColumn = c } =
      Pos { posLine = fromIntegral l, posColumn = fromIntegral c }

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage

  readCacheDb :: Make CacheDb
  readCacheDb = fmap (fromMaybe mempty) $ readJSONFile cacheDbFile

  writeCacheDb :: CacheDb -> Make ()
  writeCacheDb = writeJSONFile cacheDbFile

  cacheDbFile = outputDir </> "cache-db.json"

  version = makeVersion [0, 1, 0]
