{-# LANGUAGE OverloadedStrings #-}

-- | Common code generation utility functions
module Language.PureScript.CodeGen.Dart.Common where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Names

import System.FilePath ((</>), (<.>))
import Data.Aeson.Casing (snakeCase)
import Data.List.Split (splitOn)
import Data.Foldable (foldl')

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
identToJs :: Ident -> Text
identToJs (Ident "undefined") = "null"
identToJs (Ident name) = anyNameToJs name
identToJs (GenIdent _ _) = internalError "GenIdent in identToJs"
identToJs UnusedIdent = "$__unused"

-- This is necessary because "undefined" is effectively built-in to CoreFn.
-- https://github.com/purescript/purescript/blob/ed5fbfb75eb7d85431591d0c889fa8ada7174fd6/src/Language/PureScript/CoreFn/Desugar.hs#L92
runIdentDart :: Ident -> Text
runIdentDart (Ident "undefined") = "null"
runIdentDart (Ident i) = i
runIdentDart (GenIdent Nothing n) = "$" <> T.pack (show n)
runIdentDart (GenIdent (Just name) n) = "$" <> name <> T.pack (show n)
runIdentDart UnusedIdent = "$__unused"

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

-- | Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
identCharToText :: Char -> Text
identCharToText c | isAlphaNum c = T.singleton c
identCharToText '_' = "_"
identCharToText '.' = "$dot"
identCharToText '$' = "$dollar"
identCharToText '~' = "$tilde"
identCharToText '=' = "$eq"
identCharToText '<' = "$less"
identCharToText '>' = "$greater"
identCharToText '!' = "$bang"
identCharToText '#' = "$hash"
identCharToText '%' = "$percent"
identCharToText '^' = "$up"
identCharToText '&' = "$amp"
identCharToText '|' = "$bar"
identCharToText '*' = "$times"
identCharToText '/' = "$div"
identCharToText '+' = "$plus"
identCharToText '-' = "$minus"
identCharToText ':' = "$colon"
identCharToText '\\' = "$bslash"
identCharToText '?' = "$qmark"
identCharToText '@' = "$at"
identCharToText '\'' = "$prime"
identCharToText c = '$' `T.cons` T.pack (show (ord c))

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
