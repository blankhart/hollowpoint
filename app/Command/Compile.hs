{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command.Compile (command) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)
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
import qualified Language.PureScript.CodeGen.Dart.Options as Dart

data HollowpointOptions = HollowpointOptions
  { hpInput        :: [FilePath]
  , hpOutputDir    :: FilePath
  , hpOpts         :: Dart.Options
  , hpUsePrefix    :: Bool
  , hpJSONErrors   :: Bool
  }

-- | Arguments: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: HollowpointOptions -> IO ()
compile HollowpointOptions{..} = do
  input <- globWarningOnMisses (unless hpJSONErrors . warnFileTypeNotFound) hpInput
  when (null input && not hpJSONErrors) $ do
    hPutStr stderr $ unlines
      [ "hollowpoint compile: No input files."
      , "Usage: For basic information, try the `--help' option."
      ]
    exitFailure
  moduleFiles <- readUTF8FilesT input
  (makeErrors, makeWarnings) <- Dart.runMake hpOpts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $
          map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- Dart.inferForeignModules filePathMap
    let makeActions =
          Dart.backendMakeActions
            hpOutputDir
            filePathMap
            foreigns
            hpUsePrefix
    P.make makeActions (map snd ms)
  printWarningsAndErrors
    (Dart.optionsVerboseErrors hpOpts)
    hpJSONErrors
    makeWarnings
    makeErrors
  exitSuccess

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("hollowpoint compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths
  concatMapM f = fmap concat . mapM f

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)."

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

comments :: Opts.Parser Bool
comments = Opts.switch $
     Opts.short 'c'
  <> Opts.long "comments"
  <> Opts.help "Include comments in the generated code"

verboseErrors :: Opts.Parser Bool
verboseErrors = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose-errors"
  <> Opts.help "Display verbose error messages"

noPrefix :: Opts.Parser Bool
noPrefix = Opts.switch $
     Opts.short 'p'
  <> Opts.long "no-prefix"
  <> Opts.help "Do not include comment header"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

codegenTargets :: Opts.Parser [Dart.CodegenTarget]
codegenTargets = Opts.option targetParser $
     Opts.short 'g'
  <> Opts.long "codegen"
  <> Opts.value [Dart.Dart]
  <> Opts.help
      ( "Specifies comma-separated codegen targets to include. "
      <> targetsMessage
      <> " The default target is 'dart', but if this option is used only the targets specified will be used."
      )

targetsMessage :: String
targetsMessage = "Accepted codegen targets are '" <> intercalate "', '" (M.keys Dart.codegenTargets) <> "'."

targetParser :: Opts.ReadM [Dart.CodegenTarget]
targetParser =
  Opts.str >>= \s ->
    for (T.split (== ',') s)
      $ maybe (Opts.readerError targetsMessage) pure
      . flip M.lookup Dart.codegenTargets
      . T.unpack
      . T.strip

options :: Opts.Parser Dart.Options
options =
  Dart.Options
    <$> verboseErrors
    <*> (not <$> comments)
    <*> (handleTargets <$> codegenTargets)
  where
    -- Ensure that the JS target is included if sourcemaps are
    handleTargets :: [Dart.CodegenTarget] -> S.Set Dart.CodegenTarget
    handleTargets ts = S.fromList ts

hpOptions :: Opts.Parser HollowpointOptions
hpOptions = HollowpointOptions <$> many inputFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> hpOptions)
