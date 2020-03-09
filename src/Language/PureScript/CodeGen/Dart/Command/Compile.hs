{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.CodeGen.Dart.Command.Compile (compile) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Casing (snakeCase)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import Data.Foldable (foldl')
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

import Development.Shake

import Language.PureScript.CodeGen.Dart.Command.Options (CommandLineOptions(..))
import Language.PureScript.CodeGen.Dart.Version (versionString)

-- Return the [fully qualified] file paths.
getModuleNames :: Action [FilePath]
getModuleNames =
  fmap (takeFileName . takeDirectory) <$>
    getDirectoryFiles "" ["output/*/corefn.json"]

runShake :: Rules () -> IO ()
runShake = shake shakeOptions
  { shakeFiles=".hollowpoint"
  , shakeProgress = progressSimple
  , shakeThreads = 0
  , shakeVersion = versionString
  }

dartifyModuleName :: FilePath -> FilePath
dartifyModuleName =
    (<.> "dart")
  . (</> "index")
  . foldl' (</>) ""
  . fmap snakeCase
  . splitOn "."

compile :: CommandLineOptions -> IO ()
compile CommandLineOptions{..} = runShake $ do

  let
    pubspec =
      cloOutputDir </> "pubspec" <.> "yaml"
    getOutputLibraries = do
      cs <- fmap dartifyModuleName <$> getModuleNames
      return [cloOutputDir </> "lib" </> c | c <- cs, Just c /= cloMain]
    purescriptLibraries =
      cloOutputDir </> "lib" </> "**" </> "index" <.> "dart"

  action $ do
    when cloVersion $ putInfo $ "Version: " <> versionString
    outputFiles <- getOutputLibraries
    -- forM outputFiles $ \c -> putInfo c
    need ("foreignFiles" : outputFiles)
    need [pubspec]
    need ["pub get"]
    when cloRun $ do
      when (isJust cloMain) $ do
        need ["run"]

  purescriptLibraries %> \out -> do
    let modName = takeBaseName out
    putInfo modName
    return () -- processFile opts out ("output" </> modName </> "corefn.json")

  -- Don't overwrite this, so that the user can change it.
  -- Don't go by the package root, but require the package name and
  -- any non-lib prefix so that import statements can be determined.
  -- Or have a command to generate the pubspec file.
  pubspec %> \out -> do
    writeFileChanged out $ unlines
      [ "name: " <> cloPackageName
      ]

  phony "pub get" $ do
    return ()

  phony "run" $ do
    return ()
    -- need ["output/pskt/program.jar"]
    -- command_ [] "java" ["-jar", "output/pskt/program.jar"]

  --  Users can put foreign files in their own code, alongside PureScript, or can publish them separately, etc., as long as there are foreign files corresponding to the PS modules.  This means that the organization of the foreign input files generally should track a PS module structure.
  --
  --  There doesn't seem to be an obvious clean way to import foreign modules that have an external Dart dependency.  That would be expressed in the package's pubspec.yaml file, but the user would need to know about the dependency and import it manually.
  --
  phony "foreigns" $ do
    return ()
    {-
    let foreignOut = "output/pskt/foreigns/"
    foreignFiles <- forM (foreigns opts) $ \folder -> do
      files <- getDirectoryFiles folder ["*.kt"]
      return $ (\file -> (fileToModule file, folder </> file)) <$> files
    for_ (concat foreignFiles) $ \(modName, file) ->
      copyFileChanged file (foreignOut </> modName <.> "kt")
    -}


processFile :: CommandLineOptions -> FilePath -> FilePath -> Action ()
processFile opts outputPath coreFnPath = do
  jsonText <- T.pack <$> readFile' coreFnPath
  let modCoreFn = loadModuleFromJSON jsonText
  let modCoreImp = Dart.moduleToJs modCoreFn
  let modOutput = Dart.prettyPrintJS modCoreImp
  putInfo $ "Compiling " <> T.unpack (runModuleName $ moduleName modCoreFn)
  liftIO $ TIO.writeFile outputPath modOutput

-- Load `CoreFN` JSON representation into a `Module Ann`.
loadModuleFromJSON :: Text -> Module Ann
loadModuleFromJSON text =
  case A.parse moduleFromJSON value of
    A.Success (_, r) -> r
    _ -> internalError "failed to parse JSON value"
  where
    value = fromMaybe (internalError "ill-formatted CoreFn JSON") $
      A.decode . L.encodeUtf8 $ L.fromStrict text

internalError :: Text -> a
internalError = error . T.unpack
