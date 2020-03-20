module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.MagicDo where

import Prelude.Compat
import Protolude (ordNub)

import Debug.Trace

import Data.Maybe (fromJust, isJust)
import Data.Text (Text)

import Language.PureScript.PSString
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common (isDict)
import Language.PureScript.CodeGen.Dart.Ident
import qualified Language.PureScript.Constants as C

-- | Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }

magicDoEffect :: DartExpr -> DartExpr
magicDoEffect = magicDo C.effect C.effectDictionaries

magicDo :: Text -> C.EffectDictionaries -> DartExpr -> DartExpr
magicDo effectModule C.EffectDictionaries{..} = everywhereTopDown convert
  where
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: DartExpr -> DartExpr
  -- Desugar pure
  convert expr@(FnCall (FnCall pure' [val]) [])
    | isPure pure' =
--        trace ("Desugar pure match on " <> show expr) $
        val
  -- Desugar discard
  -- TODO: Focus attention here
  -- TODO: Shouldn't the next one take an unused parameter?  Maybe the issue is that other optimizations run first in the PS version.
  convert expr@(FnCall (FnCall bind [m]) [FnDecl Nothing [_] (Block js)])
    | isDiscard bind =
--        trace ("Desugar discard match on " <> show expr) $
        FnDecl Nothing [] $ Block (FnCall m [] : map applyReturns js )
  -- Desugar bind to wildcard
  convert expr@(FnCall (FnCall bind [m]) [FnDecl Nothing [_] (Block js)])
    | isBind bind =
--        trace ("Desugar bind to wildcard match on " <> show expr) $
        FnDecl Nothing [] $ Block (FnCall m [] : map applyReturns js )
  -- Desugar bind
  convert expr@(FnCall (FnCall bind [m]) [FnDecl Nothing [arg] (Block js)])
    | isBind bind =
--        trace ("Desugar bind match on " <> show expr) $
        FnDecl Nothing [] $
          Block ((Val arg (FnCall m [])) : map applyReturns js)
  -- Desugar untilE
  convert expr@(FnCall (FnCall f [arg]) [])
    | isEffFunc edUntil f =
--        trace ("Desugar untilE match on " <> show expr) $
        FnCall
          (FnDecl Nothing
            []
            (Block
              [ While (Unary Not (FnCall arg [])) (Block [])
              , Return (Just $ RecordLiteral []) -- return unit
              ]
            )
          )
          []
  -- Desugar whileE
  convert expr@(FnCall (FnCall (FnCall f [arg1]) [arg2]) [])
    | isEffFunc edWhile f =
--        trace ("Double applications match on " <> show expr) $
        FnCall
          (FnDecl Nothing
            []
            (Block
              [ While (FnCall arg1 []) (Block [ FnCall arg2 [] ])
              , Return (Just (RecordLiteral [])) -- return unit
              ]
            )
          )
          []
  -- Inline __do returns
  -- TODO: Revisit as there is no __do (i.e., no named function literals).
  -- Depending on the reason for this, there could be a FnDecl followed by
  -- an application.
  {-
  convert expr@(Return _ (FnCall _ (FnDecl _ (Just ident) [] body) []))
    | ident == fnName =
        trace ("Inline __do returns match on " <> show expr) $
          body
  -}
  -- Inline double applications
  convert expr@(FnCall (FnCall (FnDecl Nothing [] (Block body)) []) []) =
--    trace ("Double applications match on " <> show expr) $
      FnCall (FnDecl Nothing [] (Block (applyReturns `fmap` body))) []
  convert other =
    -- trace ("No match on " <> show effectModule) $
      other

  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (FnCall fn [dict])
    | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard expr@(FnCall (FnCall fn [dict1]) [dict2])
    | isDict (C.controlBind, C.discardUnitDictionary) dict1 &&
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
    | otherwise = trace ("Discard check: " <> show expr) $ False
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (FnCall fn [dict])
    | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind, C.discard)

  -- Check if an expression represents a function in the Effect module
  isEffFunc :: PSString -> DartExpr -> Bool
  isEffFunc name = \case
    ObjectAccessor name' (VarRef eff) ->
      (runDartIdent eff) == ("_$" <> effectModule)
        && decodeString name == Just (runDartIdent name')
    _ -> False

  applyReturns :: DartExpr -> DartExpr
  applyReturns = \case
    Return (Just ret) -> Return (Just (FnCall ret []))
    Block body -> Block (map applyReturns body)
    While cond body -> While cond (applyReturns body)
    If cond t f -> If cond (applyReturns t) (applyReturns <$> f)
    expr -> expr
