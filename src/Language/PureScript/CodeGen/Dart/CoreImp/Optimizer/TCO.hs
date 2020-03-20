module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.TCO where

import Prelude.Compat

import Debug.Trace

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
  convert = \case
    fn@(FnDecl (Just name) _ _)
      | isTailRecursive name body' ->
          replace (toLoop name outerArgs innerArgs body')
      | name == DartIdent "tryTCO" ->
          trace ("Investigated, but not tail recursive: " <> show name <> "\n\t" <> show body' <> "\n\t" <> show (countSelfReferences name body') <> show (allInTailPosition name body')) $
          fn
      | otherwise -> fn
      where
        innerArgs = headDef [] argss
        outerArgs = concat . reverse $ tailSafe argss
        (argss, body', replace) = collectAllFunctionArgs [] id fn
    expr -> expr

  --  Collect all arguments to a curried function. Keep going if the function body just returns another function, but stop if the function body is more complex. The resulting argument list will be a list of lists, but typically single-argument functions, with the innermost argument (i.e., the last parameter in the declaration) first.
  collectAllFunctionArgs :: [[DartIdent]] -> (DartExpr -> DartExpr) -> DartExpr -> ([[DartIdent]], DartExpr, DartExpr -> DartExpr)
  collectAllFunctionArgs allArgs f = \case
    FnDecl ident args (Block (body@(Return (Just _)):_)) ->
      collectAllFunctionArgs
        (args : allArgs)
        (\b -> f (FnDecl ident (map copyVar args) (Block [b])))
        body
    FnDecl ident args body@(Block _) ->
      (args : allArgs, body, f . FnDecl ident (map copyVar args))
    Return (Just (FnDecl ident args (Block [body]))) ->
      collectAllFunctionArgs
        (args : allArgs)
        (\b -> f (Return (Just (FnDecl ident (map copyVar args) (Block [b])))))
        body
    Return (Just (FnDecl ident args body@(Block _))) ->
      (args : allArgs, body, f . Return . Just . FnDecl ident (map copyVar args))
    body ->
      (allArgs, body, f)

  --  This returns a block in which the tail-call optimized variables are assigned to "copy" variables for all of the outer arguments, i.e. those that are not the function being optimized.
  --  The tail-call optimized function is created with a "loopified" body and then called with all the outer arguments and the inner arguments.
  toLoop :: DartIdent -> [DartIdent] -> [DartIdent] -> DartExpr -> DartExpr
  toLoop ident outerArgs innerArgs js = Block $
    map (\arg -> Var (tcoVar arg) (VarRef (copyVar arg))) outerArgs
    ++
    [ Var tcoDone (BooleanLiteral False)
    , Var tcoResult (VarRef "null") -- Empty initializer
    , FnDecl (Just tcoLoop) (outerArgs ++ innerArgs) (Block [loopify js])
    , While (Unary Not (VarRef tcoDone)) $ Block $
        [(VarAssign
          (VarRef tcoResult)
          (FnCall
            (VarRef tcoLoop)
            ( (map (VarRef . tcoVar) outerArgs)
              ++ (map (VarRef . copyVar) innerArgs)
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

isTailRecursive :: DartIdent -> DartExpr -> Bool
isTailRecursive ident js =
  countSelfReferences ident js > 0 && allInTailPosition ident js

countSelfReferences :: DartIdent -> DartExpr -> Int
countSelfReferences ident = getSum . everything (Sum . match)
  where
    match :: DartExpr -> Int
    match (VarRef ident') | ident == ident' = 1
    match _ = 0

allInTailPosition :: DartIdent -> DartExpr -> Bool
allInTailPosition ident = \case
  Return val -> case val of
    Just expr
      | isSelfCall ident expr -> countSelfReferences ident expr == 1
      | otherwise -> countSelfReferences ident expr == 0
    Nothing -> True
  While cond body ->
    countSelfReferences ident cond == 0 && allInTailPosition ident body
  If cond body el ->
    countSelfReferences ident cond == 0
    && allInTailPosition ident body
    && all (allInTailPosition ident) el
  Block body ->
    all (allInTailPosition ident) body
  Throw err ->
    countSelfReferences ident err == 0
  VarDecl _ _ val ->
    countSelfReferences ident val == 0
  VarAssign _ val ->
    countSelfReferences ident val == 0
  Comment _ expr ->
    allInTailPosition ident expr
  Annotation _ expr ->
    allInTailPosition ident expr
  _ -> False

