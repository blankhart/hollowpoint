module Language.PureScript.CodeGen.Dart.Command.Compile (compile) where

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import Data.Aeson.Casing (snakeCase)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import Data.Foldable (foldl')
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Development.Shake
import           Development.Shake.FilePath
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors.JSON
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)
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
compile opts = runShake $ do
  action $ do
    when (cloVersion opts) $ putNormal $ "Version: " <> versionString
    cs <- fmap dartifyModuleName <$> getModuleNames
    let outputFiles = (\c -> cloOutputDir opts </> "lib" </> c) <$> cs
    forM outputFiles $ \c -> putNormal c

    {-
    let kotlinFiles = ["output/pskt" </>  c <.> "kt" | c <- cs]
    need ["foreigns"]
    need kotlinFiles
    when (runProgram opts) $ need ["run"]

  phony "run" $ do
    need ["output/pskt/program.jar"]
    command_ [] "java" ["-jar", "output/pskt/program.jar"]

  "output/pskt/program.jar" %> \out -> do
    ktFiles <- fmap (\modName -> "output/pskt" </> modName <.> "kt") <$> getModuleNames
    need ktFiles
    need ["output/pskt/EntryPoint.kt"]
    command_
      [AddEnv "JAVA_OPTS" "-Xmx2G -Xms256M"]
      "kotlinc" $
        ["output/pskt/PsRuntime.kt", "output/pskt/EntryPoint.kt"]
        ++ ktFiles
        ++ ["output/pskt/foreigns" ]
        ++ ["-include-runtime", "-d", out]
        ++ ["-nowarn"]


  "output/pskt/PsRuntime.kt" %> \out ->
    writeFileChanged out $ unlines
      [ "@file:Suppress(\"UNCHECKED_CAST\", \"USELESS_CAST\")"
      , "package Foreign.PsRuntime;"
      , ""
      , "fun Any.app(arg: Any): Any {"
      , "   return (this as (Any) -> Any)(arg)"
      , "}"
      , ""
      , "fun Any.appRun() = (this as () -> Any)()"
      ]

  "output/pskt/EntryPoint.kt" %> \out ->
    writeFileChanged out $ unlines
      [ "@file:Suppress(\"UNCHECKED_CAST\", \"USELESS_CAST\")"
      , "import Foreign.PsRuntime.appRun;"
      , ""
      , "fun main() {"
      , "   PS.Main.Module.main.appRun()"
      , "}"
      ]

  phony "foreigns" $ do
    let foreignOut = "output/pskt/foreigns/"
    foreignFiles <- forM (foreigns opts) $ \folder -> do
      files <- getDirectoryFiles folder ["*.kt"]
      return $ (\file -> (fileToModule file, folder </> file)) <$> files
    for_ (concat foreignFiles) $ \(modName, file) ->
      copyFileChanged file (foreignOut </> modName <.> "kt")


  "output/pskt/*.kt" %> \out -> do
    let modName = takeBaseName out
    processFile opts out ("output" </> modName </> "corefn.json")

fileToModule :: FilePath -> String
fileToModule path = replaceSlash <$> path
    where
      replaceSlash '/' = '.'
      replaceSlash char = char

processFile :: CliOptions -> FilePath -> FilePath -> Action ()
processFile opts outFile path = do
  jsonText <- T.pack <$> readFile' path
  let mod = loadModuleFromJSON jsonText
  let modName = runModuleName $ moduleName mod
  let moduleKt = moduleToKt' mod
  -- pPrint moduleKt
  outputFile <- liftIO $ openFile outFile WriteMode
  putNormal $ "Transpiling " <> T.unpack modName
  let moduleDoc = moduleToText mod
  liftIO $ renderIO outputFile moduleDoc
  liftIO $ hClose outputFile

-- Load `CoreFN` JSON representation into a `Module Ann`.
loadModuleFromJSON :: Text -> Module Ann
loadModuleFromJSON value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> internalError "failed to parse JSON value"
  where
    value = fromMaybe (internalError "ill-formatted CoreFn JSON") $
      decode . L.encodeUtf8 $ L.fromStrict text
-}

internalError :: Text -> a
internalError = error . T.unpack
