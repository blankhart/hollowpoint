{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Data types for the imperative core Dart AST
module Language.PureScript.CodeGen.Dart.CoreImp.AST where

import Prelude.Compat

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity(..), runIdentity)

import Data.Functor.Contravariant (contramap)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Text (Text)

import Language.PureScript.Comments
import Language.PureScript.Traversals
import Language.PureScript.PSString (PSString)

import Language.PureScript.CodeGen.Dart.Ident

import Text.Show.Deriving (deriveShow)

-- | Data type for simplified Dart expressions
-- DAST
data DartExpr
  = Directive Directive
  -- ^ A directive.
  | NumericLiteral (Either Integer Double)
  -- ^ A numeric literal
  | StringLiteral PSString
  -- ^ A string literal
  | BooleanLiteral Bool
  -- ^ A boolean literal
  | ArrayLiteral [DartExpr]
  -- ^ An array literal
  | RecordLiteral [(PSString, DartExpr)]
  -- ^ A record literal
  | ClassDecl DartIdent [DartIdent]
  -- ^ A class declaration (name, fields)
  | FnDecl (Maybe DartIdent) [DartIdent] DartExpr
  -- ^ A FnDecl introduction (optional name, arguments, body)
  | FnCall DartExpr [DartExpr]
  -- ^ FnDecl application
  | Unary UnaryOperator DartExpr
  -- ^ A unary operator application
  | Binary BinaryOperator DartExpr DartExpr
  -- ^ A binary operator application
  | Block [DartExpr]
  -- ^ A block of expressions in braces
  | VarDecl DartIdent DartExpr
  -- ^ A variable introduction (always initialized)
  | VarRef DartIdent
  -- ^ Variable reference
  | Accessor Accessor DartExpr DartExpr
  -- ^ A collection variable accessor (collection var, field identifier)
  | VarAssign DartExpr DartExpr
  -- ^ A variable reassignment (not initialization)
  | While DartExpr DartExpr
  -- ^ While loop
  | If DartExpr DartExpr (Maybe DartExpr)
  -- ^ If-then-else statement
  | Return (Maybe DartExpr)
  -- ^ Return statement
  | Throw DartExpr
  -- ^ Throw statement
  | Comment [Comment] DartExpr
  -- ^ Commented Dart
  | Annotation Text DartExpr
  -- ^ Annotation (e.g., @pragma())
  deriving (Show, Eq)

data Directive
  = Import Text DartIdent -- ^ import "package:purescript/$1/index.dart" as $2
  | Export Text -- ^ export $1 show $2
  deriving (Show, Eq)

data Accessor
  = ArrayIndex
  -- ^ An array index (integer key)
  | RecordField
  -- ^ A record accessor expression (map key)
  | ObjectMethod
  -- ^ An object accessor expression (class instance method)
  deriving (Show, Eq)

-- | Built-in unary operators
data UnaryOperator
  = Negate
  | Not
  | BitwiseNot
  deriving (Show, Eq)

-- | Built-in binary operators
data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | EqualTo
  | NotEqualTo
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | Is
  deriving (Show, Eq)

class HasOperatorPriority a where
  priority :: a -> OperatorPriority

instance HasOperatorPriority UnaryOperator where
  priority = \case
    Negate -> NumericSumOp
    Not -> LogicalSumOp
    BitwiseNot -> BitwiseSumOp

instance HasOperatorPriority BinaryOperator where
  priority = \case
    Multiply -> NumericProductOp
    Divide -> NumericProductOp
    Modulus -> NumericProductOp
    Add -> NumericProductOp
    Subtract -> NumericSumOp
    ShiftLeft -> BitwiseShiftOp
    ShiftRight -> BitwiseShiftOp
    BitwiseAnd -> BitwiseProductOp
    BitwiseXor -> BitwiseXorOp
    BitwiseOr -> BitwiseSumOp
    EqualTo -> ComparisonOp
    NotEqualTo -> ComparisonOp
    LessThan -> ComparisonOp
    LessThanOrEqualTo -> ComparisonOp
    GreaterThan -> ComparisonOp
    GreaterThanOrEqualTo -> ComparisonOp
    Is -> ComparisonOp
    And -> LogicalProductOp
    Or -> LogicalSumOp

data OperatorPriority
  = LogicalSumOp
  | LogicalProductOp
  | ComparisonOp
  | BitwiseSumOp
  | BitwiseXorOp
  | BitwiseProductOp
  | BitwiseShiftOp
  | NumericSumOp
  | NumericProductOp
  deriving (Show, Eq, Ord)

pattern IntegerLiteral :: Integer -> DartExpr
pattern IntegerLiteral i = NumericLiteral (Left i)

pattern DoubleLiteral :: Double -> DartExpr
pattern DoubleLiteral n = NumericLiteral (Right n)

pattern Lambda :: [DartIdent] -> DartExpr -> DartExpr
pattern Lambda params body = FnDecl Nothing params body

--  TODO: Possibly wrap this in a block.
pattern Thunk :: DartExpr -> DartExpr
pattern Thunk body = Lambda [] body

pattern ArrayAccessor :: Integer -> DartExpr -> DartExpr
pattern ArrayAccessor ix var = Accessor ArrayIndex (IntegerLiteral ix) var

pattern RecordAccessor :: PSString -> DartExpr -> DartExpr
pattern RecordAccessor pss var = Accessor RecordField (StringLiteral pss) var

pattern ObjectAccessor :: DartIdent -> DartExpr -> DartExpr
pattern ObjectAccessor i var = Accessor ObjectMethod (VarRef i) var

pattern MethodCall :: DartExpr -> DartIdent -> [DartExpr] -> DartExpr
pattern MethodCall obj method args = FnCall (ObjectAccessor method obj) args

pattern IfThen :: DartExpr -> DartExpr -> DartExpr
pattern IfThen cond thens = If cond thens Nothing

pattern IfEqual :: DartExpr -> DartExpr -> DartExpr -> DartExpr
pattern IfEqual a b thens = If (Binary EqualTo a b) thens Nothing

pattern IfThenElse :: DartExpr -> DartExpr -> DartExpr -> DartExpr
pattern IfThenElse cond thens elses = If cond thens (Just elses)

pattern IIFE :: [DartExpr] -> DartExpr
pattern IIFE body = FnCall (Lambda [] (Block body)) []

-- recursion-schemes

makeBaseFunctor ''DartExpr
-- deriveShow ''DartExprF

everywhere :: (DartExpr -> DartExpr) -> DartExpr -> DartExpr
everywhere f e = cata (f . embed) e

everywhereTopDown :: (DartExpr -> DartExpr) -> DartExpr -> DartExpr
everywhereTopDown f = embed . fmap (everywhereTopDown f) . project . f

-- FIXME: Verify that this doesn't double-count.
everything :: Monoid r => (DartExpr -> r) -> DartExpr -> r
everything f e = foldMap f (project e)
