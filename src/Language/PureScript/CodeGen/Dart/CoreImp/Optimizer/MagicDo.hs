module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.MagicDo where

{-

import Prelude.Compat
import Protolude (ordNub)

import Debug.Trace

import Data.Maybe (fromJust, isJust)
import Data.Text (Text)

import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common
import Language.PureScript.PSString (mkString)
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

magicDoEffect :: AST -> AST
magicDoEffect = magicDo C.effect C.effectDictionaries

magicDo :: Text -> C.EffectDictionaries -> AST -> AST
magicDo effectModule C.EffectDictionaries{..} = everywhereTopDown convert
  where
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: AST -> AST
  -- Desugar pure
  convert expr@(App _ (App _ pure' [val]) [])
    | isPure pure' =
        trace ("Desugar pure match on " <> show expr) $
        val
  -- Desugar discard
  -- TODO: Focus attention here
  -- TODO: Shouldn't the next one take an unused parameter?  Maybe the issue is that other optimizations run first in the PS version.
  convert expr@(App _ (App _ bind [m]) [Function s1 Nothing [_] (Block s2 js)])
    | isDiscard bind =
        trace ("Desugar discard match on " <> show expr) $
        Function s1 Nothing [] $ Block s2 (App s2 m [] : map applyReturns js )
  -- Desugar bind to wildcard
  convert expr@(App _ (App _ bind [m]) [Function s1 Nothing [_] (Block s2 js)])
    | isBind bind =
        trace ("Desugar bind to wildcard match on " <> show expr) $
        Function s1 Nothing [] $ Block s2 (App s2 m [] : map applyReturns js )
  -- Desugar bind
  convert expr@(App _ (App _ bind [m]) [Function s1 Nothing [arg] (Block s2 js)])
    | isBind bind =
        trace ("Desugar bind match on " <> show expr) $
        Function s1 Nothing [] $ Block s2 (VariableIntroduction s2 arg (Just (App s2 m [])) : map applyReturns js)
  -- Desugar untilE
  convert expr@(App s1 (App _ f [arg]) [])
    | isEffFunc edUntil f =
        trace ("Desugar untilE match on " <> show expr) $
        App s1
          (Function s1 Nothing
            []
            (Block s1
              [ While s1 (Unary s1 Not (App s1 arg [])) (Block s1 [])
              , Return s1 $ RecordLiteral s1 [] -- return unit
              ]
            )
          )
          []
  -- Desugar whileE
  convert expr@(App _ (App _ (App s1 f [arg1]) [arg2]) [])
    | isEffFunc edWhile f =
        trace ("Double applications match on " <> show expr) $
        App s1
          (Function s1 Nothing
            []
            (Block s1
              [ While s1 (App s1 arg1 []) (Block s1 [ App s1 arg2 [] ])
              , Return s1 $ RecordLiteral s1 [] -- return unit
              ]
            )
          )
          []
  -- Inline __do returns
  -- More generally, apply in this case.
  {-
  convert expr@(Return _ (App _ (Function _ (Just ident) [] body) []))
    | ident == fnName =
        trace ("Inline __do returns match on " <> show expr) $
          body
  -}
  -- Inline double applications
  convert expr@(App _ (App s1 (Function s2 Nothing [] (Block ss body)) []) []) =
    trace ("Double applications match on " <> show expr) $
      App s1 (Function s2 Nothing [] (Block ss (applyReturns `fmap` body))) []
  convert other =
    -- trace ("No match on " <> show effectModule) $
      other

  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (App _ fn [dict])
    | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard expr@(App _ (App _ fn [dict1]) [dict2])
    | isDict (C.controlBind, C.discardUnitDictionary) dict1 &&
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
    | otherwise = trace ("Discard check: " <> show expr) $ False
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (App _ fn [dict])
    | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind, C.discard)
  -- Check if an expression represents a function in the Effect module
  isEffFunc name (ObjectAccessor _ (StringLiteral _ name') (Var _ eff)) =
    eff == effectModule && name == name'
  isEffFunc _ _ = False

  applyReturns :: AST -> AST
  applyReturns (Return ss ret) =
    Return ss (App ss ret [])
  applyReturns (Block ss jss) =
    Block ss (map applyReturns jss)
  applyReturns (While ss cond js) =
    While ss cond (applyReturns js)
  applyReturns (For ss v lo hi js) =
    For ss v lo hi (applyReturns js)
  applyReturns (IfElse ss cond t f) =
    IfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other
-}
