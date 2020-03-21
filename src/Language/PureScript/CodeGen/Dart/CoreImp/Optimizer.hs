module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer (
  optimize
) where

import Prelude.Compat

import Control.Monad ((>=>))
import Control.Monad.Supply.Class (MonadSupply)
import Data.Foldable (foldl')
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Inliner
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.MagicDo
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.TCO
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Unused

-- | Apply a series of optimizer passes to CoreImp DartExpr
optimize :: MonadSupply m => DartExpr -> m DartExpr
optimize =
      untilFixedPoint
        ( inlineFnComposition
        . inlineUnsafeCoerce
        . inlineUnsafePartial
        . tidyUp
        )
  >=> untilFixedPoint (return . magicDoEffect)
  >=> return . tco
  >=> untilFixedPoint (return . tidyUp)

  where
    tidyUp :: DartExpr -> DartExpr
    tidyUp = foldl' (.) id
      [ collapseNestedBlocks
      , collapseNestedIfs
      , removeCodeAfterReturnStatements
      -- NOTE: Does not interact well with MagicDo
      -- , removeNullApp
      , unThunk
      , etaConvert
      , evaluateIIFEs
      , inlineVariables
      ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
    go a = do
      a' <- f a
      if a' == a
        then return a'
        else go a'

{-
-- | Apply a series of optimizer passes to CoreImp DartExpr
optimize :: MonadSupply m => DartExpr -> m DartExpr
optimize ast = do
    ast' <- untilFixedPoint (inlineFnComposition . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp . applyAll
      [ inlineCommonValues
      , inlineCommonOperators
      ]) js
    untilFixedPoint (return . tidyUp) . tco . inlineST
      =<< untilFixedPoint (return . magicDoST)
      =<< untilFixedPoint (return . magicDoEff)
      =<< untilFixedPoint (return . magicDoEffect) ast'
  where
    tidyUp :: DartExpr -> DartExpr
    tidyUp = applyAll
      [ collapseNestedBlocks
      , collapseNestedIfs
      , removeCodeAfterReturnStatements
      , removeUndefinedApp
      , unThunk
      , etaConvert
      , evaluateIifes
      , inlineVariables
      ]
-}
