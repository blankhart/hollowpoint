{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.CodeGen.Dart.Command.Compile (compile) where

import qualified Debug.Trace as Debug

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Casing (snakeCase)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import Data.Foldable (foldl', for_)
import qualified Data.IORef as IORef
import           Data.List (intercalate, stripPrefix)
import           Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L

import           Data.Traversable (for)
import           Development.Shake
import           Development.Shake.FilePath
import Language.PureScript (runModuleName)
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hClose, hPutStr, hPutStrLn, openFile, IOMode(..), stderr)
import           System.IO.UTF8 (readUTF8FilesT)

import qualified Language.PureScript.CodeGen.Dart.CoreImp as Dart
import qualified Language.PureScript.CodeGen.Dart.Printer as Dart

import Language.PureScript.CodeGen.Dart.Common (toTargetFileName, toTargetImportName)
import Language.PureScript.CodeGen.Dart.Command.Options (CommandLineOptions(..))
import Language.PureScript.CodeGen.Dart.Version (versionString)

runShake :: Rules () -> IO ()
runShake = shake shakeOptions
  { shakeFiles=".hollowpoint"
  , shakeProgress = progressSimple
  , shakeThreads = 0
  , shakeVersion = versionString
  }

-- For now assume in the form Data/Ord.dart
psifyModuleName :: FilePath -> String
psifyModuleName =
    foldl' (\a b -> a <> "." <> b) ""
  . splitOn "/"
  . dropDirectory1 -- remove package name ("prelude/")
  . dropExtension -- remove extension (".dart")

data InMemFileMap = InMemFileMap
  { filemapDartSources :: M.Map String FilePath
  -- ^ Module name to Dart FFI file.
  , filemapDartTargets :: M.Map FilePath String
  -- ^ Dart foreign target to module.
  , filemapDartBinaries :: M.Map FilePath String
  -- ^ Dart foreign binary to module.
  , filemapPursTargets :: M.Map FilePath String
  -- ^ PureScript target to module.
  }

compile :: CommandLineOptions -> IO ()
compile opts@CommandLineOptions{..} = runShake $ do

  let

    makeOutputFileName = toTargetFileName "lib" cloPackageDir cloLibraryPrefix

    --  Compiled file is required whenever a `CoreFn` module is generated.
    pursOutputFileName =
      makeOutputFileName "index"

    --  Foreign file is required when `CoreFn` module has a non-empty `moduleForeign` field.
    dartOutputFileName =
      makeOutputFileName "foreign"

    dartBinaryFileName =
      toTargetFileName "bin" cloPackageDir cloLibraryPrefix "exe"

    --  Generate build targets
    pursOutputFileNames = pursOutputFileName "**"

    dartOutputFileNames = dartOutputFileName "**" -- lib
    dartBinaryFileNames = dartBinaryFileName "**" -- bin
    -- web

    --  Bare `pubspec.yaml` is generated when specified on the command line.
    pubspec =
      cloPackageDir </> "pubspec" <.> "yaml"

  --  TODO: Instead liftIO $ newCacheIO () with a unit key
  --  Or use newCache so that compilation depends on the contents of the PureScript output directory.
  ref <- liftIO $ IORef.newIORef (InMemFileMap mempty mempty mempty mempty)

  action $ do

    -- Print version
    when cloVersion $
      putInfo $ "Version: " <> versionString

    -- Load a map of all modules and all foreign files
    --  `spago`/`purs` generate the `CoreFn` dumps as input to the backend.
    pursModuleNames <- do
      filenames <- getDirectoryFiles "" ["output/*/corefn.json"]
      return $ fmap (takeFileName . takeDirectory) filenames

    -- Load a list of all foreign Dart FFI files
    -- (dir, filename)
    dartSourceNames <- do
      inputDirs <- liftIO $ traverse glob cloForeignInputDirs
      filenames <- forM (concat inputDirs) $ \dir -> do
        files <- getDirectoryFiles dir ["**/*.dart"]
        return $ fmap (dir,) files
      return $ concat filenames

    let

      -- Map (purs target, module name)
      pursTargets = M.fromList $
        fmap (\mn -> (pursOutputFileName mn, mn)) pursModuleNames

      -- Map (dart target, module name)
      dartTargets = M.fromList $
        fmap (\mn -> (dartOutputFileName mn, mn)) pursModuleNames

      -- Map (module name, dart source)
      dartSources = foldl' insertSourceName mempty pursModuleNames
        where
          insertSourceName acc mn = M.update (const (findSourceName mn)) mn acc

      -- Map (module name, dart source) assuming PureScript file format
      dartSourcesPursFormat = M.fromList $
        fmap (\(d, sn) -> (toModuleName sn, d </> sn)) dartSourceNames
        where
          toModuleName = intercalate "." . splitDirectories . dropExtension

      -- Map (module name, dart source) assuming Dart package format
      -- TODO: Unsupported; map module names to Dart file paths
      --       and intersect the result with observed file paths
      -- NOTE: There is no invertible mapping from Dart sources to module names
      --       in this format due to case insensitivity.
      dartSourcesDartFormat = M.fromList []

      findSourceName mn =
        M.lookup mn dartSourcesPursFormat <|> M.lookup mn dartSourcesDartFormat

      dartBinaries = case cloMain of
        Nothing -> mempty
        Just mn -> M.singleton (dartBinaryFileName mn) mn

    liftIO $ IORef.writeIORef ref InMemFileMap
      { filemapDartSources = dartSourcesPursFormat
      , filemapDartTargets = dartTargets
      , filemapDartBinaries = dartBinaries
      , filemapPursTargets = pursTargets
      }

    -- putInfo $ "Modules: " <> intercalate "," (M.elems pursTargets)
    -- putInfo $ "Sources: " <> intercalate "," (M.elems dartSources)
    -- putInfo $ "Source names: " <> show dartSourcesPursFormat

    -- Build the Dart libraries from the PureScript modules
    need $ pursOutputFileName <$> pursModuleNames

    -- Generate executables for main modules
    -- TODO: Verify that they have main() functions?
    case cloMain of
      Nothing -> return ()
      Just mn -> need [dartBinaryFileName mn]

    -- If not present, generate a `pubspec.yaml` and run `pub get`
    -- TODO: Make these opt-in or -out through command line switches
    hasPubSpec <- doesFileExist pubspec
    unless hasPubSpec $ do
      putInfo $ "Generating pubspec.yaml in " <> cloPackageDir <> "..."
      need [pubspec]
      putInfo $ "Running pub get from " <> cloPackageDir <> "..."
      command_ [Cwd cloPackageDir] "pub" ["get"]

    when cloRun $ case cloMain of
      Nothing ->
        putInfo "Error: Specifying --run also requires --main-is."
      Just mn -> do
        putInfo $ "Running binary compiled from " <> mn
        command_ [Cwd $ cloPackageDir </> ".."] "dart" [dartBinaryFileName mn]

  let
    assertValidModuleName filename = fromMaybe $ internalError $
      "No module associated with " <> (T.pack filename)
    assertValidSourceName mn = fromMaybe $ internalError $
      "No source associated with " <> (T.pack mn)

  pursOutputFileNames %> \outputPath -> do

    InMemFileMap{..} <- liftIO $ IORef.readIORef ref

    let
      mn = assertValidModuleName outputPath $
        M.lookup outputPath filemapPursTargets
      coreFnPath = "output" </> mn </> "corefn.json"

    jsonText <- T.pack <$> readFile' coreFnPath
    putInfo $ "Backend compiling " <> mn

    let
      modCoreFn = loadModuleFromJSON jsonText
      ffiSourceFileName = M.lookup mn filemapDartSources
      ffiTargetFileName = (const $ dartOutputFileName mn) <$> ffiSourceFileName
      ffiRequired = not . null $ moduleForeign modCoreFn

    -- TODO:  List input directories searched on failure.
    --        Keep the globbed list in a factored out cache.
    case ffiTargetFileName of
      Just ffi
        | not ffiRequired -> internalError $
            T.pack mn <> " has a Dart FFI file but contains no foreign declarations."
        | otherwise -> need [ffi]
      Nothing
        | ffiRequired -> internalError $
            T.pack mn <> " requires a Dart FFI file, but none was found."
        | otherwise -> return ()

    let
      modCoreImp =
        Dart.moduleToDart opts cloPackageName cloLibraryPrefix modCoreFn
      modOutput =
        Dart.prettyPrintJS modCoreImp
      modOutputDisableLints =
        "// ignore_for_file: dead_code, unused_import, unused_local_variable, omit_local_variable_types\n" <> modOutput
    writeFileChanged outputPath (T.unpack modOutputDisableLints)

  dartOutputFileNames %> \outputPath -> do
    InMemFileMap{..} <- liftIO $ IORef.readIORef ref
    let
      mn = assertValidModuleName outputPath $
        M.lookup outputPath filemapDartTargets
      sn = assertValidSourceName mn $
        M.lookup mn filemapDartSources
    putInfo $ "Copying " <> sn <> " to " <> outputPath <> "..."
    copyFileChanged sn outputPath

  pubspec %> \outputPath -> do
    writeFileChanged outputPath $ unlines
      [ "name: " <> cloPackageName
      , ""
      , "environment:"
      , "  sdk: '>=2.7.0 <3.0.0'"
      , ""
      , "dev_dependencies:"
--      , "  build_runner: ^1.6.0"
--      , "  build_web_compilers: ^2.3.0"
      ]

  {-
  <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title></title>
        <script defer src="main.dart.js"></script>
    </head>
    <body>
    </body>
  </html>
  -}

  dartBinaryFileNames %> \outputPath -> do
    InMemFileMap{..} <- liftIO $ IORef.readIORef ref
    let
      binModuleName = assertValidModuleName outputPath $
        M.lookup outputPath filemapDartBinaries
      lib =
        toTargetImportName cloPackageName cloLibraryPrefix "index" binModuleName
    writeFileChanged outputPath $ unlines
      [ "import 'package:" <> lib <> "' as ps;"
      , "void main() => ps.main();"
      ]

-- Load `CoreFN` JSON representation into a `Module Ann`.
loadModuleFromJSON :: Text -> Module Ann
loadModuleFromJSON text =
  case A.parse moduleFromJSON value of
    A.Success (_, r) -> r
    _ -> internalError "Failed to parse CoreFn JSON value."
  where
    value = fromMaybe (internalError "Found ill-formatted CoreFn JSON.") $
      A.decode . L.encodeUtf8 $ L.fromStrict text

-- TODO: Robust exceptions
internalError :: Text -> a
internalError = error . T.unpack
