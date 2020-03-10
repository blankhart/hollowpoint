{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.CodeGen.Dart.Command.Options where

import Options.Applicative

import Language.PureScript.CodeGen.Dart.Version (versionString)

import Prelude.Compat
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as Map

-- Additional options:
-- * Regenerate all
-- * Require/read existing pubspec.yaml
-- * Take cloPackageRoot, cloLibraryPrefix
--    E.g., "target" / "ps"
--    Then write files to target/lib/ps/[prelude/index.dart] (e.g.)
--    But any main-is will be written to target/bin/[main/index.dart]
-- * Really they are EXTRA foreign includes, since will want foreigns for one's own project to be kept with the PS.
-- * Dart Native (AOT) compilation
-- * Dart2Js
data CommandLineOptions = CommandLineOptions
  { cloVersion :: Bool
  -- ^ Print the program version
  , cloPackageDir :: FilePath
  -- ^ Directory for the generated Dart package
  , cloPackageName :: FilePath
  -- ^ Name of the generated Dart package
  , cloLibraryPrefix :: FilePath
  -- ^ Prefix for the generated Dart libraries ({package_name}/lib/{prefix})
  , cloMain :: Maybe FilePath
  -- ^ Main module, if any
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
  <*> strOption
    ( long "package-name"
    <> short 'p'
    <> value "target"
    <> showDefault
    <> help "Package name for the generated Dart project"
    )
  <*> strOption
    ( long "library-prefix"
    <> value "ps"
    <> showDefault
    <> help "Prefix for the generated Dart libraries"
    )
  <*> optional
    (strOption
      ( long "main-is"
      <> short 'm'
      <> help "Name of the executable module (main :: Effect Unit)"
      )
    )
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

