module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.TCO where

import Prelude.Compat

import Data.Monoid (Sum(..))
import qualified Language.PureScript.Constants as C
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.Ident
import Safe (headDef, tailSafe)

-- | Eliminate tail calls
tco :: DartExpr -> DartExpr
tco = everywhere convert where
  tcoVar :: DartIdent -> DartIdent
  tcoVar arg = "$tco_var_" <> arg

  copyVar :: DartIdent -> DartIdent
  copyVar arg = "$copy_" <> arg

  tcoDone :: DartIdent
  tcoDone = "$tco_done"

  tcoLoop :: DartIdent
  tcoLoop = "$tco_loop"

  tcoResult :: DartIdent
  tcoResult = "$tco_result"

  convert :: DartExpr -> DartExpr
  convert (VarDecl name fn@FnDecl {})
      | isTailRecursive name body'
      = VarDecl name (replace (toLoop name outerArgs innerArgs body'))
    where
      innerArgs = headDef [] argss
      outerArgs = concat . reverse $ tailSafe argss
      (argss, body', replace) = collectAllFunctionArgs [] id fn
  convert js = js

  collectAllFunctionArgs :: [[DartIdent]] -> (DartExpr -> DartExpr) -> DartExpr -> ([[DartIdent]], DartExpr, DartExpr -> DartExpr)
  collectAllFunctionArgs allArgs f (FnDecl ident args (Block (body@(Return (Just _)):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (FnDecl ident (map copyVar args) (Block [b]))) body
  collectAllFunctionArgs allArgs f (FnDecl ident args body@(Block _)) =
    (args : allArgs, body, f . FnDecl ident (map copyVar args))
  collectAllFunctionArgs allArgs f (Return (Just (FnDecl ident args (Block [body])))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (Return (Just (FnDecl ident (map copyVar args) (Block [b]))))) body
  collectAllFunctionArgs allArgs f (Return (Just (FnDecl ident args body@(Block _)))) =
    (args : allArgs, body, f . Return . Just . FnDecl ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  isTailRecursive :: DartIdent -> DartExpr -> Bool
  isTailRecursive ident js = countSelfReferences js > 0 && allInTailPosition js where
    countSelfReferences = getSum . everything (Sum . match) where
      match :: DartExpr -> Int
      match (VarRef ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition (Return val) = case val of
      Just expr
        | isSelfCall ident expr -> countSelfReferences expr == 1
        | otherwise -> countSelfReferences expr == 0
      Nothing -> True
    allInTailPosition (While cond body)
      = countSelfReferences cond == 0 && allInTailPosition body
    allInTailPosition (If cond body el)
      = countSelfReferences cond == 0 && allInTailPosition body && all allInTailPosition el
    allInTailPosition (Block body)
      = all allInTailPosition body
    allInTailPosition (Throw err)
      = countSelfReferences err == 0
    allInTailPosition (VarDecl _ val)
      = countSelfReferences val == 0
    allInTailPosition (VarAssign _ val)
      = countSelfReferences val == 0
    allInTailPosition (Comment _ expr)
      = allInTailPosition expr
    allInTailPosition (Annotation _ expr)
      = allInTailPosition expr
    allInTailPosition _
      = False

  toLoop :: DartIdent -> [DartIdent] -> [DartIdent] -> DartExpr -> DartExpr
  toLoop ident outerArgs innerArgs js = Block $
    map (\arg -> VarDecl (tcoVar arg) (VarRef (copyVar arg))) outerArgs
    ++
    [ VarDecl tcoDone (BooleanLiteral False)
    , VarDecl tcoResult (VarRef "null") -- Empty initializer
    , FnDecl (Just tcoLoop) (outerArgs ++ innerArgs) (Block [loopify js])
    , While (Unary Not (VarRef tcoDone)) $ Block $
        [(VarAssign
          (VarRef tcoResult)
          (FnCall
            (VarRef tcoLoop)
            ((map (VarRef . tcoVar) outerArgs) ++ (map (VarRef . copyVar) innerArgs)
            )
          )
        )]
    , Return (Just (VarRef tcoResult))
    ]

    where

    loopify :: DartExpr -> DartExpr
    loopify (Return val) = case val of
      Just ret
        | isSelfCall ident ret ->
          let
            allArgumentValues = concat $ collectArgs [] ret
          in
            Block $
              zipWith (\val arg ->
                VarAssign (VarRef (tcoVar arg)) val) allArgumentValues outerArgs
              ++ zipWith (\val arg ->
                VarAssign (VarRef (copyVar arg)) val) (drop (length outerArgs) allArgumentValues) innerArgs
              ++ [ Return Nothing ]
        | otherwise -> Block [ markDone, Return (Just ret) ]
      Nothing ->
        Block [ markDone, Return Nothing ]
    loopify (While cond body) = While cond (loopify body)
    loopify (If cond body el) = If cond (loopify body) (fmap loopify el)
    loopify (Block body) = Block (map loopify body)
    loopify other = other

    markDone :: DartExpr
    markDone =
      VarAssign (VarRef tcoDone) (BooleanLiteral True)

    collectArgs :: [[DartExpr]] -> DartExpr -> [[DartExpr]]
    collectArgs acc (FnCall fn []) =
      -- count 0-argument applications as single-argument so we get the correct number of args
      collectArgs ([VarRef C.undefined] : acc) fn
    collectArgs acc (FnCall fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

  isSelfCall :: DartIdent -> DartExpr -> Bool
  isSelfCall ident (FnCall (VarRef ident') _) = ident == ident'
  isSelfCall ident (FnCall fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
