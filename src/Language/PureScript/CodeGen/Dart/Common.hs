{-# LANGUAGE OverloadedStrings #-}

-- | Common code generation utility functions
module Language.PureScript.CodeGen.Dart.Common where

import Debug.Trace

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Names

import System.FilePath ((</>), (<.>))
import Data.Aeson.Casing (snakeCase)
import Data.List.Split (splitOn)
import Data.Foldable (foldl')

-- TODO: Clean a la  https://github.com/csicar/pskt/blob/408f66f55231882f981c03baa69e26921f7ea806/src/CodeGen/KtCore.hs#L121

toDartFilePath :: FilePath -> FilePath
toDartFilePath =
    foldl' (</>) ""
  . fmap snakeCase
  . splitOn "."

toTargetFileName :: String -> FilePath -> FilePath -> FilePath -> String -> FilePath
toTargetFileName dir packageDir libraryPrefix baseName moduleName =
  packageDir
  </> dir -- lib, web, bin
  </> libraryPrefix
  </> toDartFilePath moduleName
  </> baseName
  <.> "dart"

toTargetImportName :: String -> FilePath -> FilePath -> String -> FilePath
toTargetImportName packageName libraryPrefix baseName moduleName =
  packageName
  </> libraryPrefix
  </> toDartFilePath moduleName
  </> baseName
  <.> "dart"

moduleNameToJs :: ModuleName -> Text
moduleNameToJs (ModuleName pns) =
  let name = T.intercalate "_" (runProperName `map` pns)
  in if nameIsJsBuiltIn name then "$$" <> name else name

-- | Convert an 'Ident' into a valid JavaScript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
-- This is necessary because "undefined" is effectively built-in to CoreFn.
-- https://github.com/purescript/purescript/blob/ed5fbfb75eb7d85431591d0c889fa8ada7174fd6/src/Language/PureScript/CoreFn/Desugar.hs#L92identToJs :: Ident -> Text
identToJs (Ident "undefined") = "null"
-- NOTE: Some references to $__unused come through CoreFn
identToJs (Ident "$__unused") = "_"
identToJs (Ident name)
  -- make all identifiers public by default
  | Just n <- T.stripPrefix "_" name = "$_" <> anyNameToJs n
  | otherwise = anyNameToJs name
identToJs (GenIdent _ _) = internalError "GenIdent in identToJs"
identToJs UnusedIdent = "_"

{-
runIdentDart :: Ident -> Text
runIdentDart (Ident "undefined") = "null"
runIdentDart (Ident i) = i
runIdentDart (GenIdent Nothing n) = "$" <> T.pack (show n)
runIdentDart (GenIdent (Just name) n) = "$" <> name <> T.pack (show n)
runIdentDart UnusedIdent = "$__unused"
-}

-- | Convert a 'ProperName' into a valid JavaScript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
properToJs :: ProperName a -> Text
properToJs = anyNameToJs . runProperName

-- | Convert any name into a valid JavaScript identifier.
--
-- Note that this function assumes that the argument is a valid PureScript
-- identifier (either an 'Ident' or a 'ProperName') to begin with; as such it
-- will not produce valid JavaScript identifiers if the argument e.g. begins
-- with a digit. Prefer 'identToJs' or 'properToJs' where possible.
anyNameToJs :: Text -> Text
anyNameToJs name
  | nameIsJsReserved name || nameIsJsBuiltIn name = "$$" <> name
   -- should be a null set for proper names, and handled elsewhere for value identifiers, but make all identifiers public by default:
  | Just underscored <- T.stripPrefix "_" name = "$_" <> anyNameToJs underscored
  | otherwise = T.concatMap identCharToText name

-- | Test if a string is a valid JavaScript identifier as-is. Note that, while
-- a return value of 'True' guarantees that the string is a valid JS
-- identifier, a return value of 'False' does not guarantee that the string is
-- not a valid JS identifier. That is, this check is more conservative than
-- absolutely necessary.
isValidJsIdentifier :: Text -> Bool
isValidJsIdentifier s =
  and
    [ not (T.null s)
    , isAlpha (T.head s)
    , s == anyNameToJs s
    ]

-- | Attempts to find a human-readable name for a symbol, if none has been specified returns the ordinal value.
--  TODO: The only symbols in this list that appear to be valid PureScript identifiers are the prime and the underscore.
identCharToText :: Char -> Text
identCharToText '\'' = "$prime"
identCharToText c = T.singleton c

-- | Checks whether an identifier name is reserved in JavaScript.
nameIsJsReserved :: Text -> Bool
nameIsJsReserved name =
  name `elem` jsAnyReserved

-- | Checks whether a name matches a built-in value in JavaScript.
nameIsJsBuiltIn :: Text -> Bool
nameIsJsBuiltIn name =
  name `elem`
    [ "arguments"
    , "Array"
    , "ArrayBuffer"
    , "Boolean"
    , "DataView"
    , "Date"
    , "decodeURI"
    , "decodeURIComponent"
    , "encodeURI"
    , "encodeURIComponent"
    , "Error"
    , "escape"
    , "eval"
    , "EvalError"
    , "Float32Array"
    , "Float64Array"
    , "Function"
    , "Infinity"
    , "Int16Array"
    , "Int32Array"
    , "Int8Array"
    , "Intl"
    , "isFinite"
    , "isNaN"
    , "JSON"
    , "Map"
    , "Math"
    , "NaN"
    , "Number"
    , "Object"
    , "parseFloat"
    , "parseInt"
    , "Promise"
    , "Proxy"
    , "RangeError"
    , "ReferenceError"
    , "Reflect"
    , "RegExp"
    , "Set"
    , "SIMD"
    , "String"
    , "Symbol"
    , "SyntaxError"
    , "TypeError"
    , "Uint16Array"
    , "Uint32Array"
    , "Uint8Array"
    , "Uint8ClampedArray"
    , "unescape"
    , "URIError"
    , "WeakMap"
    , "WeakSet"
    ]

jsAnyReserved :: [Text]
jsAnyReserved =
  concat
    [ jsKeywords
    , jsSometimesReserved
    , jsFutureReserved
    , jsFutureReservedStrict
    , jsOldReserved
    , jsLiterals
    ]

jsKeywords :: [Text]
jsKeywords =
  [ "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "export"
  , "extends"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  ]

jsSometimesReserved :: [Text]
jsSometimesReserved =
  [ "await"
  , "let"
  , "static"
  , "yield"
  ]

jsFutureReserved :: [Text]
jsFutureReserved =
  [ "enum" ]

jsFutureReservedStrict :: [Text]
jsFutureReservedStrict =
  [ "implements"
  , "interface"
  , "package"
  , "private"
  , "protected"
  , "public"
  ]

jsOldReserved :: [Text]
jsOldReserved =
  [ "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "double"
  , "final"
  , "float"
  , "goto"
  , "int"
  , "long"
  , "native"
  , "short"
  , "synchronized"
  , "throws"
  , "transient"
  , "volatile"
  ]

jsLiterals :: [Text]
jsLiterals =
  [ "null"
  , "true"
  , "false"
  ]
