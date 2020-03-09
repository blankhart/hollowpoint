{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.CodeGen.Dart.String
( prettyPrintString
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Language.PureScript.PSString (PSString(..))
import Numeric (showHex)
import Prelude

newtype DartString = DartString Text deriving (Show, Eq)

-- |
-- Pretty print a PSString, using Dart escape sequences. Intended for
-- use in compiled Dart output.
-- FIXME: Adapt to Dart strings
--
prettyPrintString :: PSString -> Text
prettyPrintString s = "\"" <> foldMap encodeChar (toUTF16CodeUnits s) <> "\""
  where
  encodeChar :: Word16 -> Text
  encodeChar c | c > 0xFF = "\\u" <> showHex' 4 c
  encodeChar c | c > 0x7E || c < 0x20 = "\\x" <> showHex' 2 c
  encodeChar c | toChar c == '\b' = "\\b"
  encodeChar c | toChar c == '\t' = "\\t"
  encodeChar c | toChar c == '\n' = "\\n"
  encodeChar c | toChar c == '\v' = "\\v"
  encodeChar c | toChar c == '\f' = "\\f"
  encodeChar c | toChar c == '\r' = "\\r"
  encodeChar c | toChar c == '"'  = "\\\""
  encodeChar c | toChar c == '\\' = "\\\\"
  encodeChar c = Text.singleton $ toChar c

showHex' :: Enum a => Int -> a -> Text
showHex' width c =
  let hs = showHex (fromEnum c) "" in
  Text.pack (replicate (width - length hs) '0' <> hs)

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral
