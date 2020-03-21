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
    isReturn = \case { Return{} -> True; _ -> False }

-- NOTE: Is this advantageous in some fashion, either at runtime
-- or for later optimizations?  The function takes a null parameter
-- that is presumably unused.
removeNullApp :: DartExpr -> DartExpr
removeNullApp = everywhere $ \case
  FnCall fn [VarRef arg] | arg == "null" -> FnCall fn []
  expr -> expr
