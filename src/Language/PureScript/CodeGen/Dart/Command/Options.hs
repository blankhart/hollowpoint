module Language.PureScript.CodeGen.Dart.Command.Options where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Language.PureScript.CodeGen.Dart.Version (versionString)

import Prelude.Compat
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as Map

data CommandLineOptions = CommandLineOptions
  { cloVersion :: Bool
  -- ^ Print the program version
  , cloOutputDir :: FilePath
  -- ^ Directory for the generated Dart package
  , cloMainIs :: Maybe Text
  -- ^ Main module
  , cloRun :: Bool
  -- ^ Run the program
  , cloStripComments :: Bool
  -- ^ Remove comments from  generated Dart
  , cloForeignInputDirs :: [FilePath]
  -- ^ Directory of the foreign (Dart) input files
  }

cliOptions :: Parser CommandLineOptions
cliOptions = CommandLineOptions
  <$> switch
    ( long "version"
    <> short 'v'
    <> help "Print the hollowpoint version"
    )
  <*> strOption
    ( long "output"
    <> short 'o'
    <> value "target"
    <> showDefault
    <> help "Package directory for the generated Dart files"
    )
  <*> optional (strOption
    ( long "main-is"
    <> short 'm'
    <> help "Package directory for the generated Dart files"
    ))
  <*> switch
    ( long "run"
    <> help "Run the program in interpreted mode"
    )
  <*> switch
    ( long "strip-comments"
    <> help "Strip comments from the generated Dart"
    )
  <*> many
    ( option str
      ( long "foreigns"
      <> help "Package directories for foreign files"
      )
    )

cliParser :: ParserInfo CommandLineOptions
cliParser = info (cliOptions <**> helper)
  (  fullDesc
  <> progDesc ("hollowpoint " <> versionString)
  <> header "Dart backend for PureScript"
  )

