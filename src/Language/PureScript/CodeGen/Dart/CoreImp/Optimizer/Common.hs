module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common where

import Prelude.Compat

import Data.Text (Text)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..))

import Language.PureScript.Crash
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.Ident
import Language.PureScript.PSString (PSString, decodeString)

import Debug.Trace

replaceIdent :: DartIdent -> DartExpr -> DartExpr -> DartExpr
replaceIdent var1 js = everywhere replace
  where
  replace (VarRef var2) | var1 == var2 = js
  replace other = other

replaceIdents :: [(DartIdent, DartExpr)] -> DartExpr -> DartExpr
replaceIdents vars = everywhere replace
  where
  replace v@(VarRef var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: DartIdent -> DartExpr -> Bool
isReassigned var1 = getAny . everything (Any . check)
  where
  check :: DartExpr -> Bool
  check (FnDecl _ args _) | var1 `elem` args = True
  check (VarDecl arg _) | var1 == arg = True
  check (VarAssign (VarRef arg) _) | var1 == arg = True
  check _ = False

isRebound :: DartExpr -> DartExpr -> Bool
isRebound js d =
  any (\v -> isReassigned v d || isUpdated v d) (everything variablesOf js)
  where
    variablesOf (VarRef var) = [var]
    variablesOf _ = []

isUsed :: DartIdent -> DartExpr -> Bool
isUsed var1 = getAny . everything (Any . check)
  where
  check :: DartExpr -> Bool
  check (VarRef var2) | var1 == var2 = True
  check (VarAssign target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: DartExpr -> DartIdent
targetVariable (VarRef var) = var
targetVariable (Accessor _ _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: DartIdent -> DartExpr -> Bool
isUpdated var1 = getAny . everything (Any . check)
  where
  check :: DartExpr -> Bool
  check (VarAssign target _) | var1 == targetVariable target = True
  check _ = False

-- Inliner, Unused
removeFromBlock :: ([DartExpr] -> [DartExpr]) -> DartExpr -> DartExpr
removeFromBlock go (Block sts) = Block (go sts)
removeFromBlock _  js = js

-- MagicDo
-- NOTE: Rewritten on the assumption only relevant to typeclass dictionaries.
-- FIXME: This is fragile.  The constants in the PureScript code base bake in
-- the way the printer translates module names.  Use the same translation
-- function rather than hard-coding "_$" here.
isDict :: (Text, PSString) -> DartExpr -> Bool
isDict (moduleName, dictName) (ObjectAccessor x (VarRef y)) =
  Just (runDartIdent x) == decodeString dictName
  && runDartIdent y == ("_$" <> moduleName)
isDict _ _ = False

isDict' :: [(Text, PSString)] -> DartExpr -> Bool
isDict' xs js = any (`isDict` js) xs
