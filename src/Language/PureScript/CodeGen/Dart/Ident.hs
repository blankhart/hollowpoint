{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.CodeGen.Dart.Ident where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Names

import System.FilePath ((</>), (<.>))
import Data.Aeson.Casing (snakeCase)
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.String (IsString(..))

-- | Dart identifiers
newtype DartIdent = DartIdent { runDartIdent :: Text }
  deriving (Show, Eq, IsString, Monoid, Semigroup)

-- | Directory recognized by Dart pub tool
data DartDir = Lib | Bin | Web
  deriving (Eq)

instance Show DartDir where
  show = \case
    Lib -> "lib"
    Bin -> "bin"
    Web -> "web"

data DartBaseFile = Index | Foreign
  deriving (Eq)

instance Show DartBaseFile where
  show = \case
    Index -> "index"
    Foreign -> "foreign"

-- TODO: Clean a la  https://github.com/csicar/pskt/blob/408f66f55231882f981c03baa69e26921f7ea806/src/CodeGen/KtCore.hs#L121

-- Data.BigThing -> data/big_thing
toDartFilePath :: FilePath -> FilePath
toDartFilePath =
    foldl' (</>) ""
  . fmap snakeCase
  . splitOn "."

-- Data.BigThing -> _$Data_BigThing
fromModuleName :: ModuleName -> DartIdent
fromModuleName (ModuleName pns) = DartIdent $
  "_$" <> T.intercalate "_" (runProperName `map` pns)

toTargetFileName :: DartDir -> FilePath -> FilePath -> DartBaseFile -> String -> FilePath
toTargetFileName dir packageDir libraryPrefix baseName moduleName =
  packageDir
  </> show dir -- lib, web, bin
  </> libraryPrefix
  </> toDartFilePath moduleName
  </> show baseName
  <.> "dart"

toTargetImportName :: String -> FilePath -> DartBaseFile -> String -> FilePath
toTargetImportName packageName libraryPrefix baseName moduleName =
  packageName
  </> libraryPrefix
  </> toDartFilePath moduleName
  </> show baseName
  <.> "dart"

-- | Convert an 'Ident' into a valid Dart identifier:
--  * Alphanumeric characters are kept unmodified.
--  * Reserved identifiers are prefixed with '$$'.
--  * "undefined" is special cased because it is built into CoreFn.
-- https://github.com/purescript/purescript/blob/ed5fbfb75eb7d85431591d0c889fa8ada7174fd6/src/Language/PureScript/CoreFn/Desugar.hs#L92identToJs :: Ident -> Text
-- NOTE: Some references to $__unused come through CoreFn
-- NOTE: Make all used identifiers public by default
fromIdent :: Ident -> DartIdent
fromIdent = \case
  Ident "undefined" -> "null"
  Ident "$__unused" -> "_"
  Ident name
    | Just n <- T.stripPrefix "_" name -> "$_" <> fromAnyName n
    | otherwise -> fromAnyName name
  GenIdent _ _ -> internalError "GenIdent in identToJs"
  UnusedIdent -> "_"

fromProperName :: ProperName a -> DartIdent
fromProperName = fromAnyName . runProperName

-- | Convert any name into a valid JavaScript identifier.
--
-- Note that this function assumes that the argument is a valid PureScript
-- identifier (either an 'Ident' or a 'ProperName') to begin with; as such it
-- will not produce valid Dart identifiers if the argument e.g. begins
-- with a digit. Prefer 'identToJs' or 'properToJs' where possible.
fromAnyName :: Text -> DartIdent
fromAnyName name
  | isReserved name || isBuiltIn name =
      DartIdent ("$$" <> name)
   -- should be a null set for proper names, and handled elsewhere for value identifiers, but make all identifiers public by default:
  | Just underscored <- T.stripPrefix "_" name =
      DartIdent "$_" <> fromAnyName underscored
  | otherwise =
      DartIdent (T.concatMap identCharToText name)

-- There may be circumstances in which an arbitrary string would be used as
-- an object accessor, and so would need to be converted.  Think further.
-- This is because record keys can have arbitrary names and be looked up
-- with string literals.  But this implementation uses objects only for
-- sum and product types (which have simple generated field names) and for
-- type classes whose methods must be valid PureScript identifiers.
identCharToText :: Char -> Text
identCharToText '\'' = "$prime"
identCharToText c = T.singleton c

-- | Checks whether an identifier name is reserved in JavaScript.
isReserved :: Text -> Bool
isReserved name = elem name []

-- | Checks whether a name matches a built-in value in JavaScript.
isBuiltIn :: Text -> Bool
isBuiltIn name = elem name []
