module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Inliner where

{-

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.PSString (PSString)
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common
import Language.PureScript.AST (SourceSpan(..))
import qualified Language.PureScript.Constants as C

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: AST -> AST
collapseNestedBlocks = everywhere collapse
  where
    collapse :: AST -> AST
    collapse (Block ss sts) = Block ss (concatMap go sts)
    collapse js = js

    go :: AST -> [AST]
    go (Block _ sts) = sts
    go s = [s]

collapseNestedIfs :: AST -> AST
collapseNestedIfs = everywhere collapse
  where
    collapse :: AST -> AST
    collapse (IfElse _ (BooleanLiteral _ True) (Block _ [js]) _) = js
    collapse (IfElse s1 cond1 (Block _ [IfElse s2 cond2 body Nothing]) Nothing) =
        IfElse s1 (Binary s2 And cond1 cond2) body Nothing
    collapse js = js

shouldInline :: AST -> Bool
shouldInline (Var _ _) = True
shouldInline (NumericLiteral _ _) = True
shouldInline (StringLiteral _ _) = True
shouldInline (BooleanLiteral _ _) = True
shouldInline (ArrayIndexer _ index val) = shouldInline index && shouldInline val
shouldInline (RecordAccessor _ index val) = shouldInline index && shouldInline val
shouldInline (ObjectAccessor _ index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: AST -> AST
etaConvert = everywhere convert
  where
  --  When within a block, instead of returning an anonymous function applied to literals, return the body of the function substituting the literals for the corresponding parameters.  Note, this may fail to catch for curried functions.
  -- TODO: Why is this complicated?
  convert :: AST -> AST
  convert (Block ss [Return _ (App _ (Function _ Nothing idents block@(Block _ body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map (Var Nothing) idents)) &&
      not (any (`isRebound` block) args)
      = Block ss (map (replaceIdents (zip idents args)) body)
  -- Parameterless function that simply calls parameterless function
  convert (Function _ Nothing [] (Block _ [Return _ (App _ fn [])])) = fn
  convert js = js

unThunk :: AST -> AST
unThunk = everywhere convert
  where
  convert :: AST -> AST
  -- Preserve empty blocks
  convert (Block ss []) = Block ss []
  -- If the last statement in a block just applies a parameterless lambda,
  -- pull the function body out for direct inclusion in the block.
  -- pattern Thunk :: AST -> AST
  -- Thunk body = Function _ Nothing [] (Block _ body)
  convert (Block ss jss) =
    case last jss of
      Return _ (App _ (Function _ Nothing [] (Block _ body)) []) ->
        Block ss $ init jss ++ body
      _ -> Block ss jss
  convert js = js

-- pattern IIFE :: AST -> AST
-- IIFE body =
--    App _ (Function _ Nothing [] (Block _ [Return _ body])) []
--  TODO: Understand the case where the function takes nonzero parameters but is applied with zero arguments. The way to do this would be to look back at moduleToJs to see when this is created and why.
evaluateIifes :: AST -> AST
evaluateIifes = everywhere convert
  where
  convert :: AST -> AST
  convert (App _ (Function _ Nothing [] (Block _ [Return _ ret])) []) = ret
  convert (App _ (Function _ Nothing idents (Block _ [Return ss ret])) [])
    | not (any (`isReassigned` ret) idents) =
        replaceIdents (map (, Var ss C.undefined) idents) ret
  convert js = js

-- If the variable "should inline" then eliminate the declaration, and
-- replace references to the variable with the underlying literal
-- But confirm that it is not reassigned, updated, or "rebound" (?)
inlineVariables :: AST -> AST
inlineVariables = everywhere $ removeFromBlock go
  where
  go :: [AST] -> [AST]
  go [] = []
  go (VariableIntroduction _ var (Just js) : sts)
    | shouldInline js && not (any (isReassigned var) sts) && not (any (isRebound js) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var js) sts)
  go (s:sts) = s : go sts

-}
