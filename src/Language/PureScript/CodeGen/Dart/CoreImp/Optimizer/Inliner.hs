module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Inliner where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.PSString (PSString)
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common (isReassigned, isRebound, isUpdated, removeFromBlock, replaceIdent, replaceIdents)
import qualified Language.PureScript.Constants as C

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: DartExpr -> DartExpr
collapseNestedBlocks = everywhere collapse
  where
    collapse :: DartExpr -> DartExpr
    collapse (Block sts) = Block (concatMap go sts)
    collapse js = js

    go :: DartExpr -> [DartExpr]
    go (Block sts) = sts
    go s = [s]

collapseNestedIfs :: DartExpr -> DartExpr
collapseNestedIfs = everywhere collapse
  where
    collapse :: DartExpr -> DartExpr
    collapse = \case
      If (BooleanLiteral True) (Block [js]) _ ->
        js
      If cond1 (Block [If cond2 body Nothing]) Nothing ->
        If (Binary And cond1 cond2) body Nothing
      other ->
        other

shouldInline :: DartExpr -> Bool
shouldInline = \case
  VarRef{} -> True
  NumericLiteral{} -> True
  StringLiteral{} -> True
  BooleanLiteral{} -> True
  Accessor _ acc val -> shouldInline acc && shouldInline val
  _ -> False

etaConvert :: DartExpr -> DartExpr
etaConvert = everywhere convert
  where
  --  When within a block, instead of returning an anonymous function applied to literals, return the body of the function substituting the literals for the corresponding parameters.  Note, this may fail to catch for curried functions.
  -- TODO: Why is this complicated?
  convert :: DartExpr -> DartExpr
  convert = \case
    Block [Return (Just (FnCall (FnDecl Nothing idents block@(Block body)) args))]
      | all shouldInline args &&
        not (any (`isRebound` block) (map VarRef idents)) &&
        not (any (`isRebound` block) args)
        -> Block (map (replaceIdents (zip idents args)) body)
    -- Parameterless function that simply calls parameterless function
    FnDecl Nothing [] (Block [Return (Just (FnCall fn []))]) -> fn
    expr -> expr

-- NOTE: Revisit, and possibly use safe last
unThunk :: DartExpr -> DartExpr
unThunk = everywhere convert
  where
  convert :: DartExpr -> DartExpr
  convert = \case
    Block [] -> Block []
    -- If the last statement in a block just applies a parameterless lambda,
    -- pull the function body out for direct inclusion in the block.
    -- pattern Thunk :: DartExpr -> DartExpr
    -- Thunk body = Function _ Nothing [] (Block _ body)
    Block sts ->
      case last sts of
        Return (Just (FnCall (FnDecl Nothing [] (Block body)) [])) ->
          Block $ init sts ++ body
        _ -> Block sts
    expr -> expr

-- pattern IIFE :: DartExpr -> DartExpr
-- IIFE body =
--    App _ (Function _ Nothing [] (Block _ [Return _ body])) []
--  TODO: Understand the case where the function takes nonzero parameters but is applied with zero arguments. The way to do this would be to look back at moduleToJs to see when this is created and why.
evaluateIifes :: DartExpr -> DartExpr
evaluateIifes = everywhere convert
  where
  convert :: DartExpr -> DartExpr
  convert = \case
    FnCall (FnDecl Nothing [] (Block [Return (Just ret)])) [] -> ret
    FnCall (FnDecl Nothing idents (Block [Return (Just ret)])) []
      | not (any (`isReassigned` ret) idents) ->
          replaceIdents (map (, VarRef C.undefined) idents) ret
    expr -> expr

-- If the variable "should inline" then eliminate the declaration, and
-- replace references to the variable with the underlying literal
-- But confirm that it is not reassigned, updated, or "rebound" (?)
inlineVariables :: DartExpr -> DartExpr
inlineVariables = everywhere $ removeFromBlock go
  where
    go :: [DartExpr] -> [DartExpr]
    go = \case
      [] -> []
      (Val val expr) : sts
        | shouldInline expr
          && not (any (isReassigned val) sts)
          && not (any (isRebound expr) sts)
          && not (any (isUpdated val) sts)
          -> go (map (replaceIdent val expr) sts)
      s:sts -> s : go sts
