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
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
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

import           Language.PureScript.CodeGen.Dart.Make.Actions as Dart
import           Language.PureScript.CodeGen.Dart.Make.Foreigns as Dart
import           Language.PureScript.CodeGen.Dart.Make.Monad as Dart

import qualified Language.PureScript.CodeGen.Dart.CoreImp2 as Dart
import qualified Language.PureScript.CodeGen.Dart.Printer as Dart


import Language.PureScript.CodeGen.Dart.Command.Options (CommandLineOptions(..))
import Language.PureScript.CodeGen.Dart.Version (versionString)

runShake :: Rules () -> IO ()
runShake = shake shakeOptions
  { shakeFiles=".hollowpoint"
  , shakeProgress = progressSimple
  , shakeThreads = 0
  , shakeVersion = versionString
  }

-- Replace chars rather than split and rejoin
dartifyModuleName :: FilePath -> FilePath -> FilePath
dartifyModuleName name =
    (<.> "dart")
  . (</> name)
  . foldl' (</>) ""
  . fmap snakeCase
  . splitOn "."

-- For now assume in the form Data/Ord.dart
psifyModuleName :: FilePath -> String
psifyModuleName =
    foldl' (\a b -> a <> "." <> b) ""
  . splitOn "/"
  . dropDirectory1 -- remove package name ("prelude/")
  . dropExtension -- remove extension (".dart")

compile :: CommandLineOptions -> IO ()
compile CommandLineOptions{..} = runShake $ action $ do

  let

    --  Possibly, use the shake system by creating a mapping between the module name/the PureScript output file, and the module name/Dart FFI output file, on each build. Then shake rules can be used while efficiently looking up the relevant mappings.

    --  If there is a main module or modules, then generate a package_dir/bin/main module file that just forwards to the library main file.

    --  If the snake case conventions are honored, then module names may not be invertible due to case sensitivity.  Dart requires file names to be snake cased because some file systems are not case sensitive.  In general, PureScript libraries should avoid collisions based on case sensitivity.

    --  `spago`/`purs` generate the `CoreFn` dumps as input to the backend.
    getModuleNames =
      fmap (takeFileName . takeDirectory) <$>
        getDirectoryFiles "" ["output/*/corefn.json"]

    makeOutputFileName base mn =
      cloPackageDir </> "lib" </> cloLibraryPrefix </> dartifyModuleName base mn

    --  Compiled file is required whenever a `CoreFn` module is generated.
    pursOutputFileName = makeOutputFileName "index"

    --  Foreign file is required when `CoreFn` module has a non-empty `moduleForeign` field.
    dartOutputFileName = makeOutputFileName "foreign"

    --  Bare `pubspec.yaml` is generated when specified on the command line.
    pubspec =
      cloOutputDir </> "pubspec" <.> "yaml"

    makeTargetMapping mn (pursTargets, dartTargets) =


  -- Print version
  when cloVersion $ putInfo $ "Version: " <> versionString

  putInfo $
    "Looking for foreign files at: "
    <> intercalate "," cloForeignInputDirs

  -- Load a map of all modules and all foreign files
  moduleNames <- getModuleNames

  pursTargets = M.fromList $
    fmap (\mn -> (pursOutputFileName mn, mn) moduleNames

  dartTargets = M.fromList $
    fmap (\mn -> (dartOutputFileName mn, mn) moduleNames

  --  This is the key invertibility issue - the sources either have to have the target name already, so that they can be findable by reverse mapping the targets (swapping "foreign" for "index") or have the same names as the PureScript files.  These can be passed on the command line separately as "side load" or "direct load."
  dartSources = M.fromList $
    fmap (\mn -> (mn, source))

  --  These should be run only when the PureScript output has changed, i.e. when there has been a change to:
  --  * The `CoreFn` output for a PureScript module.
  --  * The Dart FFI file corresponding to a PureScript module.
  for moduleNames $ \mn ->
    -- load from text
    -- Equivalent of inferForeignModules/checkForeignDecls
    -- verify if module has foreign bindings
    -- if it has a foreign file, need the foreign file
    -- * if not found but needed, raise an error
    -- * if found but not needed, raise a warning
    -- verify that the foreign file has the necessary Dart declarations
    -- * if any missing, specify which
    -- if foreign file is found and validated, pass the path to compiler

  foreignFiles <- forM cloForeignInputDirs $ \dir -> do
    files <- getDirectoryFiles dir ["**/*.dart"]
    let modules = psifyModuleName <$> files
        qualifieds = dartifyModuleName "foreign" <$> modules
        targets = (\f -> cloOutputDir </> "lib" </> f) <$> qualifieds
        sources = fmap (dir </>) files
    return $ zip sources targets
  for_ (concat foreignFiles) $ \(source, target) -> do
    putInfo $ "Copying " <> source <> " to " <> target <> "..."
    copyFileChanged source target

  -- If specified, generate a `pubspec.yaml`
  -- Don't go by the package root, but require the package name and
  -- any non-lib prefix so that import statements can be determined.
  -- Or have a command to generate the pubspec file.
  writeFileChanged pubspec $ unlines
    [ "name: " <> cloPackageName
    ]

  -- If specified, run the output
  when cloRun $ do
    when (isJust cloMain) $ do
      return ()

  -- Run pub get by default
  return ()

processFile :: CommandLineOptions -> FilePath -> FilePath -> Action ()
processFile opts outputPath coreFnPath = do
  jsonText <- T.pack <$> readFile' coreFnPath
  let modCoreFn = loadModuleFromJSON jsonText
  let modCoreImp = Dart.moduleToDart opts Nothing modCoreFn
  let modOutput = Dart.prettyPrintJS modCoreImp
  putInfo $ "Compiling " <> T.unpack (runModuleName $ moduleName modCoreFn)
  writeFileChanged (T.unpack modOutput) outputPath

-- Load `CoreFN` JSON representation into a `Module Ann`.
loadModuleFromJSON :: Text -> Module Ann
loadModuleFromJSON text =
  case A.parse moduleFromJSON value of
    A.Success (_, r) -> r
    _ -> internalError "failed to parse JSON value"
  where
    value = fromMaybe (internalError "ill-formatted CoreFn JSON") $
      A.decode . L.encodeUtf8 $ L.fromStrict text

-- TODO: Robust exceptions
internalError :: Text -> a
internalError = error . T.unpack
