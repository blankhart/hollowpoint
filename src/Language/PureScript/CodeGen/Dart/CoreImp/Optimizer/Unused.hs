module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Unused where

import Prelude.Compat

import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common
import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: DartExpr -> DartExpr
removeCodeAfterReturnStatements = everywhere (removeFromBlock go)
  where
    go :: [DartExpr] -> [DartExpr]
    go jss
      | not (any isReturn jss) = jss
      | otherwise = let (body, ret : _) = break isReturn jss in body ++ [ret]
    isReturn Return{} = True
    isReturn _ = False

removeUndefinedApp :: DartExpr -> DartExpr
removeUndefinedApp = everywhere $ \case
  FnCall fn [VarRef arg]
    | arg == C.undefined -> FnCall fn []
  expr -> expr
