-- | Pretty printer for the Dart AST
module Language.PureScript.CodeGen.Dart.Printer
  ( prettyPrintJS
  , prettyPrintJSWithSourceMaps
  ) where

import Prelude.Compat

-- FIXME: Remove, but note absence of exhaustivity checks with this API.
-- import Debug.Trace (traceShow)

import Control.Arrow ((<+>))
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Pretty.Common
import Language.PureScript.PSString (PSString, decodeString)
import Language.PureScript.CodeGen.Dart.Common
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.String as Dart

-- TODO (Christoph): Get rid of T.unpack / pack

literals :: (Emit gen) => Pattern PrinterState AST gen
literals = mkPattern' match'
  where
  match' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
  match' js = (addMapping' (getSourceSpan js) <>) <$> match js

  match :: (Emit gen) => AST -> StateT PrinterState Maybe gen

  -- FIXME: Package system, module naming, etc.
  match (Directive _ d) = return $ case d of
    Library lib -> emit $ "library " <> lib
    Import lib qual -> emit $ "import \"" <> lib <> "\" as " <> qual
    Export lib -> emit $ "export \"" <> lib <> "\""
    Pragma p -> emit $ "@pragma('" <> p <> "')"

  match (NumericLiteral _ n) = return $ emit $ T.pack $ either show show n

  match (StringLiteral _ s) = return $ emit $ Dart.prettyPrintString s

  match (BooleanLiteral _ True) = return $ emit "true"

  match (BooleanLiteral _ False) = return $ emit "false"

  match (ArrayLiteral _ xs) = mconcat <$> sequence
    [ return $ emit "[ "
    , intercalate (emit ", ") <$> forM xs prettyPrintJS'
    , return $ emit " ]"
    ]

  match (RecordLiteral _ []) = return $ emit "{}"

  match (RecordLiteral _ ps) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) ->
          fmap ((toRecordKey key <> emit ": ") <>) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate (emit ",\n") $ map (indentString <>) jss
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]
    where
    toRecordKey :: (Emit gen) => PSString -> gen
    toRecordKey s = emit "'" <> emit key <> emit "'"
      where
      -- TODO: Review whether the identifier restriction is needed
      -- if these are Map<String, dynamic>
      key = case decodeString s of
        Just s' | isValidJsIdentifier s' -> s'
        _ -> Dart.prettyPrintString s

  match (Block _ sts) = mconcat <$> sequence
    [ return $ emit "{\n"
    , withIndent $ prettyStatements sts
    , return $ emit "\n"
    , currentIndent
    , return $ emit "}"
    ]

  match (Var _ ident) = return $ emit ident

  match (VariableIntroduction _ ident value) = mconcat <$> sequence
    [ return $ emit $ case value of
        -- This is just because the Dart analyzer cannot infer the type of
        -- a block-bodied function.  The Dart common front end is said to have
        -- this capability.
        Just (Function _ _ _ _) -> "final dynamic " <> ident
        _ -> "final " <> ident
    , maybe (return mempty) (fmap (emit " = " <>) . prettyPrintJS') value
    ]

  match (Assignment _ target value) = mconcat <$> sequence
    [ prettyPrintJS' target
    , return $ emit " = "
    , prettyPrintJS' value
    ]

  match (While _ cond sts) = mconcat <$> sequence
    [ return $ emit "while ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' sts
    ]

  match (For _ ident start end sts) = mconcat <$> sequence
    [ return $ emit $ "for (var " <> ident <> " = "
    , prettyPrintJS' start
    , return $ emit $ "; " <> ident <> " < "
    , prettyPrintJS' end
    , return $ emit $ "; " <> ident <> "++) "
    , prettyPrintJS' sts
    ]

  match (IfElse _ cond thens elses) = mconcat <$> sequence
    [ return $ emit "if ("
    , prettyPrintJS' cond
    , return $ emit ") "
    , prettyPrintJS' thens
    , maybe (return mempty) (fmap (emit " else " <>) . prettyPrintJS') elses
    ]

  match (Return _ value) = mconcat <$> sequence
    [ return $ emit "return "
    , prettyPrintJS' value
    ]

  match (ReturnNoResult _) = return $ emit "return"

  match (Throw _ value) = mconcat <$> sequence
    [ return $ emit "throw "
    , prettyPrintJS' value
    ]

  match (Comment _ com js) = mconcat <$> sequence
    [ return $ emit "\n"
    , mconcat <$> forM com comment
    , prettyPrintJS' js
    ]

  -- FIXME: Ignoring superclasses (types)
  -- return $ emit "@sealed" -- only relevant to type, not constructor
  -- and so would decorate a parent abstract class
  -- FIXME: Review whether nullaries should have a static member, since "const constructors" do not return constant objects (they are just capable of doing so).
  -- FIXME: Longer-term, possible use of enums for appropriate types.
  match (ClassDeclaration _ (ConcreteClass c) _ fields) =
    mconcat <$> sequence
      [ return $ emit ("class " <> c <> " {\n")
      , withIndent $ do
          i <- currentIndent
          mconcat <$> sequence
            [ return $ mconcat $ flip fmap fields $ \field ->
              i <> emit ("final dynamic " <> field <> ";\n")
            , let initializers = intercalate ", " (fmap ("this." <>) fields)
              in  return $
                    i <> emit "const " <> emit (c <> "(" <> initializers <> ");\n")
            -- The curried constructor.  Note, typeclass constructors will
            -- always be applied fully saturated and so don't need this, even
            -- though they take multiple arguments.
            , let
                ind n = T.replicate n "    "
                call = c <> "(" <> T.intercalate ", " fields <> ");\n"
                (ctor, _) = foldr curried (call, length fields) fields
                curried arg (acc, n) = (acc', n - 1)
                  where
                    acc' = "(" <> arg <> ") {\n"
                      <> ind (n + 1) <> "return " <> acc
                      <> ind n <> "};\n"
              in if null fields
                  then return $ emit ""
                  else
                    return $
                      i <> emit ("static dynamic get create => " <> ctor)
            , return $ emit "}"
            ]
      ]

  -- NOTE: This can produce an infinite loop if it fails to match.
  match _ = mzero
  -- match q = traceShow q $ mzero

  comment :: (Emit gen) => Comment -> StateT PrinterState Maybe gen
  comment (LineComment com) = fmap mconcat $ sequence $
    [ currentIndent
    , return $ emit "//" <> emit com <> emit "\n"
    ]
  comment (BlockComment com) = fmap mconcat $ sequence $
    [ currentIndent
    , return $ emit "/**\n"
    ] ++
    map asLine (T.lines com) ++
    [ currentIndent
    , return $ emit " */\n"
    , currentIndent
    ]
    where
    asLine :: (Emit gen) => Text -> StateT PrinterState Maybe gen
    asLine s = do
      i <- currentIndent
      return $ i <> emit " * " <> (emit . removeComments) s <> emit "\n"

    removeComments :: Text -> Text
    removeComments t =
      case T.stripPrefix "*/" t of
        Just rest -> removeComments rest
        Nothing -> case T.uncons t of
          Just (x, xs) -> x `T.cons` removeComments xs
          Nothing -> ""

-- FIXME
recordAccessor :: Pattern PrinterState AST (Text, AST)
recordAccessor = mkPattern match
  where
  match (RecordAccessor _ (StringLiteral _ prop) val) =
    case decodeString prop of
      Just s | isValidJsIdentifier s -> Just (s, val)
      _ -> Nothing
  match _ = Nothing

objectAccessor :: Pattern PrinterState AST (Text, AST)
objectAccessor = mkPattern match
  where
  match (ObjectAccessor _ (StringLiteral _ prop) val) =
    case decodeString prop of
      Just s | isValidJsIdentifier s -> Just (s, val)
      _ -> Nothing
  match _ = Nothing

-- FIXME
indexer :: (Emit gen) => Pattern PrinterState AST (gen, AST)
indexer = mkPattern' match
  where
  match (ArrayIndexer _ index val) = (,) <$> prettyPrintJS' index <*> pure val
  match (RecordAccessor _ index val) = (,) <$> prettyPrintJS' index <*> pure val
  match (ObjectAccessor _ index val) = (,) <$> prettyPrintJS' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState AST ((Maybe Text, [Text], Maybe SourceSpan), AST)
lam = mkPattern match
  where
  match (Function ss name args ret) = Just ((name, args, ss), ret)
  match _ = Nothing

app :: (Emit gen) => Pattern PrinterState AST (gen, AST)
app = mkPattern' match
  where
  match (App _ val args) = do
    jss <- traverse prettyPrintJS' args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

is :: Pattern PrinterState AST (AST, AST)
is = mkPattern match
  where
  match (Is _ val ty) = Just (val, ty)
  match _ = Nothing

unary' :: (Emit gen) => UnaryOperator -> (AST -> Text) -> Operator PrinterState AST gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState AST (gen, AST)
  match = mkPattern match'
    where
    match' (Unary _ op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState AST gen
unary op str = unary' op (const str)

negateOperator :: (Emit gen) => Operator PrinterState AST gen
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (Unary _ Negate _) = True
  isNegate _ = False

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState AST gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState AST (AST, AST)
  match = mkPattern match'
    where
    match' (Binary _ op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

-- This adds semicolons after every statement.
prettyStatements :: (Emit gen) => [AST] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  let declFlags = map isDecl sts
      isDecl (ClassDeclaration _ _ _ _) = True
      isDecl _ = False
      fmtStmt js decl = case decl of
        True -> indentString <> js
        False -> indentString <> js <> emit ";"
  return $ intercalate (emit "\n") $
    zipWith fmtStmt jss declFlags

-- | Generate a pretty-printed string representing a collection of JavaScript expressions at the same indentation level
prettyPrintJSWithSourceMaps :: [AST] -> (Text, [SMap])
prettyPrintJSWithSourceMaps js =
  let StrPos (_, s, mp) = (fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements) js
  in (s, mp)

prettyPrintJS :: [AST] -> Text
prettyPrintJS = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

-- | Generate an indented, pretty-printed string representing a JavaScript expression
prettyPrintJS' :: (Emit gen) => AST -> StateT PrinterState Maybe gen
prettyPrintJS' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState AST gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState AST gen
  operators =
    OperatorTable [ [ Wrap indexer $ \index val ->
                        val <> emit "[" <> index <> emit "]" ]
                  , [ Wrap recordAccessor $ \prop val ->
                        val <> emit "['" <> emit prop <> emit "']" ]
                  , [ Wrap objectAccessor $ \prop val ->
                        val <> emit "." <> emit prop ]
                  , [ Wrap app $ \args val ->
                        val <> emit "(" <> args <> emit ")" ]
                  , [ Wrap lam $ \(name, args, ss) ret ->
                        addMapping' ss <>
                          emit (fromMaybe "" name
                          <> "(" <> intercalate ", " args <> ") ")
                          <> ret ]
                  , [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR is $ \v1 v2 -> v1 <> emit " is " <> v2 ]
                  , [ binary    EqualTo              "=="
                    , binary    NotEqualTo           "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                    ]
