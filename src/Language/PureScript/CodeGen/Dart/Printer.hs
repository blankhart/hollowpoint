module Language.PureScript.CodeGen.Dart.Printer (printModule) where

import Control.Monad (forM, mzero)

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Word (Word16)

import Language.PureScript.Comments
import Language.PureScript.Crash
import qualified Language.PureScript.Pretty.Common as P
import Language.PureScript.PSString (PSString, decodeString, toUTF16CodeUnits)

import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.Ident

import Numeric (showHex)

-- TODO: Provide renderings to both Text and IO.
-- TODO: Be lazy with Text and renderLazy.
-- See https://hackage.haskell.org/package/prettyprinter-1.6.1/docs/Data-Text-Prettyprint-Doc-Render-Text.html
printModule :: [DartExpr] -> Text
printModule decls =
  renderStrict $ layoutPretty defaultLayoutOptions $ vsep $
    "// ignore_for_file: avoid_init_to_null, dead_code, unused_import, unused_local_variable, non_constant_identifier_names, omit_local_variable_types, top_level_function_literal_block"
    : (semicolonize <$> decls)

semicolonize :: DartExpr -> Doc ann
semicolonize e = case e of
  FnDecl _ _ inner
    | isFatArrowizable inner -> statement
    | otherwise -> declaration
  FnCall{} -> statement
  VarDecl{} -> statement
  VarAssign{} -> statement
  Return{} -> statement
  Throw{} -> statement
  Directive{} -> statement
  _ -> expression
  where
    statement = pretty e <> ";"
    declaration = pretty e
    expression = pretty e

instance Pretty DartExpr where
  pretty = \case

    Directive (Import lib name) ->
      "import" <+> squotes (pretty lib) <+> "as" <+> pretty name

    Directive (Export lib) ->
      "export" <+> squotes (pretty lib)

    IntegerLiteral i ->
      pretty i

    -- TODO: Review whether this always produces correct Dart
    DoubleLiteral d ->
      pretty d

    StringLiteral ps ->
      pretty ps

    BooleanLiteral b -> case b of
      True -> "true"
      False -> "false"

    ArrayLiteral es ->
      -- TODO: Break across lines in appropriate cases
      brackets $ hsep $ punctuate "," (pretty <$> es)

    RecordLiteral kvs -> case kvs of
      [] -> "{}"
      ps -> braces $ align $ vsep $ punctuate comma $ assign <$> kvs
      where
        assign (key, value) = pretty key <+> ":" <+> pretty value

    ClassDecl name fields ->
      "class" <+> pretty name <+> blockedDecls bodyDecls
      where
        bodyDecls =
          fieldDecls ++ [ctorDecl, createDecl]
        fieldOuts =
          fmap pretty fields
        fieldDecls =
          fmap (\f -> "final dynamic" <+> pretty f <> ";") fields
        ctorDecl =
          "const" <+> pretty name <> uncurriedArgs (fmap ("this." <>) fields) <> ";"
        createDecl =
          let
            args = if null fields then "" else curriedArgs fields <+> "=>"
          in
            "static dynamic get" <+> "create" <+> "=>" <+> args <+> pretty name <> uncurriedArgs fields <> ";"

    -- TODO: Integrate with AST pass that converts single-expression functions
    -- into lambdas.
    FnDecl fn args body
      | isFatArrowizable body ->
          name $ uncurriedArgs args <> " => " <> pretty body
      | otherwise ->
          name $ uncurriedArgs args <> pretty body
      where
        name = maybe ("" <>) (\n -> (pretty n <+>)) fn

    FnCall fn args ->
      pretty fn <> uncurriedArgs args

    Unary op e ->
      pretty op <> smartParens op e

    Binary op lhs rhs ->
      smartParens op lhs <+> pretty op <+> smartParens op rhs

    Block decls ->
      blockedDecls (semicolonize <$> decls)

    VarDecl mut name expr ->
      modifier <+> pretty name <+> "=" <+> pretty expr
      where
        modifier = case mut of
          -- Reassignment required by TCO optimization
          Mutable -> "var"
          -- Literals are compile-time constants, unless mutable
          -- Reassignment not necessary for ordinary variable
          Immutable -> case expr of
            StringLiteral{} -> "const"
            NumericLiteral{} -> "const"
            BooleanLiteral{} -> "const"
            _ -> "final"

    VarRef name ->
      pretty name

    Accessor ArrayIndex index name ->
      pretty name <> brackets (pretty index)

    Accessor RecordField field name ->
      pretty name <> brackets (pretty field)

    Accessor ObjectMethod method name ->
      pretty name <> "." <> pretty method

    VarAssign name expr ->
      pretty name <+> "=" <+> pretty expr

    While cond body ->
      "while" <+> parens (pretty cond) <+> pretty body

    If cond thens elses ->
      "if" <+> parens (pretty cond)
        <> pretty thens
        <> maybe "" (\e -> "else" <+> pretty e) elses

    Return e ->
      "return" <+> pretty e

    Throw e ->
      "throw" <+> pretty e

    Comment coms decl ->
      vsep (pretty <$> coms) <> line <> pretty decl

    Annotation ann e ->
      "@" <> pretty ann <> line <> pretty e

instance Pretty UnaryOperator where
  pretty op = pretty (t :: Text) where
    t = case op of
      Negate -> "-"
      Not -> "!"
      BitwiseNot -> "~"

instance Pretty BinaryOperator where
  pretty op = pretty (t :: Text) where
    t = case op of
      -- Precedence class
      Multiply -> "*"
      Divide -> "/"
      Modulus -> "%"
      -- Precedence class
      Add -> "+"
      Subtract -> "-"
      -- Precedence class
      ShiftLeft -> "<<"
      ShiftRight -> ">>"
      -- Precedence class
      BitwiseAnd -> "&"
      -- Precedence class
      BitwiseXor -> "^"
      -- Precedence class
      BitwiseOr -> "|"
      -- Precedence class
      EqualTo -> "=="
      NotEqualTo -> "!="
      LessThan -> "<"
      LessThanOrEqualTo -> "<="
      GreaterThan -> ">"
      GreaterThanOrEqualTo -> ">="
      Is -> "is"
      -- Precedence class
      And -> "&&"
      Or -> "||"

-- | Pretty-print a PSString using Dart escape sequences.
-- Intended for use in compiled Dart output.
-- FIXME: Adapt to Dart strings
instance Pretty PSString where
  pretty s = pretty ("\'" <> foldMap encodeChar (toUTF16CodeUnits s) <> "\'")
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
      encodeChar c = T.singleton $ toChar c

      showHex' :: Enum a => Int -> a -> Text
      showHex' width c =
        let hs = showHex (fromEnum c) "" in
        T.pack (replicate (width - length hs) '0' <> hs)

      toChar :: Word16 -> Char
      toChar = toEnum . fromIntegral

instance Pretty Comment where
  pretty = \case
    LineComment com -> "//" <+> pretty com
    BlockComment com -> "/**" <> line <> vsep clines <> line <> "*/"
      where
        clines = fmap (pretty . (" * " <>)) (T.lines com)
    -- FIXME: Ensure block comments does not include "*/"

instance Pretty DartIdent where
  pretty = pretty . runDartIdent

uncurriedArgs :: Pretty a => [a] -> Doc ann
uncurriedArgs =
  parens . hsep . punctuate "," . fmap pretty

curriedArgs :: Pretty a => [a] -> Doc ann
curriedArgs =
  concatWith (surround " => ") . fmap (parens . pretty)

blockedDecls :: [Doc ann] -> Doc ann
blockedDecls decls =
  braces $ line <> indent margin (align (vsep decls)) <> line

smartParens :: HasOperatorPriority o => o -> DartExpr -> Doc ann
smartParens op expr = case expr of
  Unary o _ | priority o <= priority op -> parens (pretty expr)
  Binary o _ _ | priority o <= priority op -> parens (pretty expr)
  _ -> pretty expr

-- TODO: Make this configurable
margin :: Int
margin = 2
