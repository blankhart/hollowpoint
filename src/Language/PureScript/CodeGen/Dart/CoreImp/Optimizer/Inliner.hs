module Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Inliner where

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Either (rights)
import Data.Foldable (foldl')
import Data.String (IsString)
import Data.Text (Text)

import Language.PureScript.PSString (PSString)
import Language.PureScript.CodeGen.Dart.CoreImp.AST
import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer.Common (isDict, isDict', isReassigned, isRebound, isUpdated, removeFromBlock, replaceIdent, replaceIdents)
import Language.PureScript.CodeGen.Dart.Ident
import qualified Language.PureScript.Constants.Prelude as C

collapseExpressionBlocks :: DartExpr -> DartExpr
collapseExpressionBlocks = everywhere $ \case
  FnDecl fn args (Block [Ret expr])
    | isFatArrowizable expr
      -> FnDecl fn args expr
  expr -> expr

collapseNestedBlocks :: DartExpr -> DartExpr
collapseNestedBlocks = everywhere $ \case
  Block sts -> Block (concatMap go sts)
  expr -> expr
  where
    go :: DartExpr -> [DartExpr]
    go = \case
      Block sts -> sts
      expr -> [expr]

collapseNestedIfs :: DartExpr -> DartExpr
collapseNestedIfs = everywhere $ \case
  If (BooleanLiteral True) (Block [sts]) _ ->
    sts
  If (BooleanLiteral False) _ (Just (Block [sts])) ->
    sts
  If cond1 (Block [If cond2 body Nothing]) Nothing ->
    If (Binary And cond1 cond2) body Nothing
  expr ->
    expr

shouldInline :: DartExpr -> Bool
shouldInline = \case
  VarRef{} -> True
  NumericLiteral{} -> True
  StringLiteral{} -> True
  BooleanLiteral{} -> True
  Accessor _ acc val -> shouldInline acc && shouldInline val
  _ -> False

etaConvert :: DartExpr -> DartExpr
etaConvert = everywhere $ \case
  --  When within a block, instead of returning an anonymous function applied to literals, return the body of the function substituting the literals for the corresponding parameters.
  --  NOTE: How does this work with curried functions.
  Block [Ret (FnCall (FnDecl Nothing idents block@(Block body)) args)]
    | all shouldInline args &&
      not (any ((`isRebound` block) . VarRef) idents) &&
      not (any (`isRebound` block) args)
      -> Block (map (replaceIdents (zip idents args)) body)
  -- Parameterless function that simply calls parameterless function
  -- In principle this could be expanded to any case where the argument
  -- lists are the same.  Note, the `fn` value to FnCall will be a FnDecl.
  FnDecl Nothing [] (Block [Ret (FnCall fn [])]) -> fn
  expr -> expr

-- NOTE: Revisit, and possibly use safe last
unThunk :: DartExpr -> DartExpr
unThunk = everywhere $ \case
  Block [] -> Block []
  -- If the last statement in a block just applies a parameterless lambda,
  -- pull the function body out for direct inclusion in the block.
  -- pattern Thunk :: DartExpr -> DartExpr
  -- Thunk body = Function _ Nothing [] (Block _ body)
  Block sts ->
    case last sts of
      Ret (FnCall (FnDecl Nothing [] (Block body)) []) ->
        Block $ init sts ++ body
      _ -> Block sts
  expr -> expr

--  TODO: Understand the case where the function takes nonzero parameters but is applied with zero arguments.
evaluateIIFEs :: DartExpr -> DartExpr
evaluateIIFEs = everywhere $ \case
  IIFE [] [Ret ret] -> ret
  IIFE idents [Ret ret]
    | not (any (`isReassigned` ret) idents) ->
        replaceIdents (map (, VarRef "null") idents) ret
  expr -> expr

-- If the variable "should inline" then eliminate the declaration, and
-- replace references to the variable with the underlying literal
-- But confirm that it is not reassigned, updated, or rebound.
-- This may dupliate work that the Dart analyzer would perform.
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

inlineUnsafeCoerce :: DartExpr -> DartExpr
inlineUnsafeCoerce = everywhereTopDown $ \case
  FnCall (ObjectAccessor unsafeCoerceFn (VarRef unsafeCoerce)) [ comp ]
    | (unsafeCoerceFn, unsafeCoerce) == (C.unsafeCoerceFn, C.unsafeCoerce)
      -> comp
  expr -> expr

-- TODO: JavaScript backend applied to C.undefined while this applies to null.
-- The expectation is that the application will be optimized away if doing so
-- is safe.  Make sure this happens.
inlineUnsafePartial :: DartExpr -> DartExpr
inlineUnsafePartial = everywhereTopDown $ \case
  FnCall (ObjectAccessor unsafePartial (VarRef partialUnsafe)) [ comp ]
    | (unsafePartial, partialUnsafe) == (C.unsafePartial, C.partialUnsafe)
      -> FnCall comp [ VarRef "null" ]
  expr -> expr

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: forall m. MonadSupply m => DartExpr -> m DartExpr
inlineFnComposition = everywhereTopDownM $ \case
  FnCall (FnCall (FnCall (FnCall fn [dict']) [x]) [y]) [z]
    | isFnCompose dict' fn -> return $ FnCall x [FnCall y [z]]
    | isFnComposeFlipped dict' fn -> return $ FnCall y [FnCall x [z]]
  app@(FnCall (FnCall (FnCall fn [dict']) _) _)
    | isFnCompose dict' fn || isFnComposeFlipped dict' fn
      -> mkApps <$> goApps app <*> freshName
  expr -> return expr

  where

    mkApps :: [Either DartExpr (Text, DartExpr)] -> Text -> DartExpr
    mkApps fns a = FnCall (FnDecl Nothing [] (Block $ vals <> [Ret comp])) []
      where
        vals = (\(t, e) -> Val (fromAnyName t) e) <$> rights fns
        comp = FnDecl Nothing [name] (Block [Ret apps])
        apps = foldr (\fn acc -> FnCall (mkApp fn) [acc]) (VarRef name) fns
        name = fromAnyName a

    mkApp :: Either DartExpr (Text, DartExpr) -> DartExpr
    mkApp = either id $ \(name, _) -> VarRef (fromAnyName name)

    goApps :: DartExpr -> m [Either DartExpr (Text, DartExpr)]
    goApps (FnCall (FnCall (FnCall fn [dict']) [x]) [y])
      | isFnCompose dict' fn = mappend <$> goApps x <*> goApps y
      | isFnComposeFlipped dict' fn = mappend <$> goApps y <*> goApps x
    goApps app@FnCall{} = pure . Right . (,app) <$> freshName
    goApps other = pure [Left other]

    isFnCompose :: DartExpr -> DartExpr -> Bool
    isFnCompose dict' fn =
      isDict (C.controlSemigroupoid, C.semigroupoidFn) dict'
      && isDict (C.controlSemigroupoid, C.compose) fn

    isFnComposeFlipped :: DartExpr -> DartExpr -> Bool
    isFnComposeFlipped dict' fn =
      isDict (C.controlSemigroupoid, C.semigroupoidFn) dict'
      && isDict (C.controlSemigroupoid, C.composeFlipped) fn

-- NOTE: Some of these are integer operations intended to optimize
-- numerical computation in JavaScript by or-ing with zero. These
-- aren't necessary in Dart.
inlineCommonValues :: DartExpr -> DartExpr
inlineCommonValues = everywhere $ \case

  FnCall fn [dict]
    | isDict' [semiringNumber, semiringInt] dict
      && isDict (C.dataSemiring, C.zero) fn
      -> IntegerLiteral 0
    | isDict' [semiringNumber, semiringInt] dict
      && isDict (C.dataSemiring, C.one) fn
      -> IntegerLiteral 1
    | isDict boundedBoolean dict
      && isDict (C.dataBounded, C.bottom) fn
      -> BooleanLiteral False
    | isDict boundedBoolean dict
      && isDict (C.dataBounded, C.top) fn
      -> BooleanLiteral True

  FnCall (FnCall fn [dict]) [x]
    | isDict ringInt dict
      && isDict (C.dataRing, C.negate) fn
      -> Unary Negate x

  FnCall (FnCall (FnCall fn [dict]) [x]) [y]
    | isDict semiringInt dict
      && isDict (C.dataSemiring, C.add) fn
      -> Binary Add x y
    | isDict semiringInt dict
      && isDict (C.dataSemiring, C.mul) fn
      -> Binary Multiply x y
    | isDict ringInt dict
      && isDict (C.dataRing, C.sub) fn
      -> Binary Subtract x y

  expr -> expr

inlineCommonOperators :: DartExpr -> DartExpr
inlineCommonOperators = everywhereTopDown $ foldl' (.) id
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate

  , binary euclideanRingNumber opDiv Divide

  , binary eqNumber opEq EqualTo
  , binary eqNumber opNotEq NotEqualTo
  , binary eqInt opEq EqualTo
  , binary eqInt opNotEq NotEqualTo
  , binary eqString opEq EqualTo
  , binary eqString opNotEq NotEqualTo
  , binary eqChar opEq EqualTo
  , binary eqChar opNotEq NotEqualTo
  , binary eqBoolean opEq EqualTo
  , binary eqBoolean opNotEq NotEqualTo

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqualTo
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqualTo
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqualTo
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqualTo
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqualTo
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

  , binary semigroupString opAppend Add

  , binary heytingAlgebraBoolean opConj And
  , binary heytingAlgebraBoolean opDisj Or
  , unary  heytingAlgebraBoolean opNot Not

  , binary' C.dataIntBits C.or BitwiseOr
  , binary' C.dataIntBits C.and BitwiseAnd
  , binary' C.dataIntBits C.xor BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
--  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot

  , inlineNonClassFunction (isModFn (C.dataFunction, C.apply)) $
      \f x -> FnCall f [x]
  , inlineNonClassFunction (isModFn (C.dataFunction, C.applyFlipped)) $
      \x f -> FnCall f [x]
  , inlineNonClassFunction (isModFnWithDict (C.dataArray, C.unsafeIndex)) $
      \f x -> Accessor ArrayIndex x f
  ] {- ++
  [ fn
    | i <- [0..10]
    , fn <- [ mkFn i
            , runFn i
            ]
  ] ++
  [ fn
    | i <- [0..10]
    , fn <- [ mkEffFn C.controlMonadEffUncurried C.mkEffFn i
            , runEffFn C.controlMonadEffUncurried C.runEffFn i
            ]
  ] ++
  [ fn
    | i <- [0..10]
    , fn <- [ mkEffFn C.effectUncurried C.mkEffectFn i
            , runEffFn C.effectUncurried C.runEffectFn i
            ]
  ] -}

binary :: (Text, PSString) -> (Text, PSString) -> BinaryOperator -> DartExpr -> DartExpr
binary dict fns op = \case
  FnCall (FnCall (FnCall fn [dict']) [x]) [y]
    | isDict dict dict'
      && isDict fns fn
      -> Binary op x y
  expr -> expr

binary' :: Text -> PSString -> BinaryOperator -> DartExpr -> DartExpr
binary' moduleName opString op = \case
  FnCall (FnCall fn [x]) [y]
    | isDict (moduleName, opString) fn
      -> Binary op x y
  expr -> expr

unary :: (Text, PSString) -> (Text, PSString) -> UnaryOperator -> DartExpr -> DartExpr
unary dicts fns op = \case
  FnCall (FnCall fn [dict']) [x]
    | isDict dicts dict'
      && isDict fns fn
      -> Unary op x
  expr -> expr

unary' :: Text -> PSString -> UnaryOperator -> DartExpr -> DartExpr
unary' moduleName fnName op = \case
  FnCall fn [x]
    | isDict (moduleName, fnName) fn
      -> Unary op x
  expr -> expr

inlineNonClassFunction :: (DartExpr -> Bool) -> (DartExpr -> DartExpr -> DartExpr) -> DartExpr -> DartExpr
inlineNonClassFunction p f = \case
  FnCall (FnCall op' [x]) [y] | p op' -> f x y
  expr -> expr

isModFn :: (Text, PSString) -> DartExpr -> Bool
isModFn (m, op) = isDict (m, op)

isModFnWithDict :: (Text, PSString) -> DartExpr -> Bool
isModFnWithDict (m, op) = \case
  FnCall fn [VarRef _] -> isDict (m, op) fn
  _ -> False

{-
mkFn :: Int -> DartExpr -> DartExpr
mkFn = mkFn' C.dataFunctionUncurried C.mkFn $ \_ _ _ args js ->
  Lambda args (Block [Ret js])

mkEffFn :: Text -> Text -> Int -> DartExpr -> DartExpr
mkEffFn modName fnName = mkFn' modName fnName $ \ss1 ss2 ss3 args js ->
  Lambda args (Block [Ret (FnCall js [])])

mkFn' :: Text -> Text -> (Maybe SourceSpan -> Maybe SourceSpan -> Maybe SourceSpan -> [Text] -> DartExpr -> DartExpr) -> Int -> DartExpr -> DartExpr
mkFn' modName fnName res 0 = \case
  FnCall mkFnN [Lambda [_] (Block [Ret js])]
    | isNFn modName fnName 0 mkFnN
      -> res s1 s2 s3 [] js
  expr -> expr
mkFn' modName fnName res n = \case
  orig@(FnCall mkFnN [fn])
    | isNFn modName fnName n mkFnN -> case collectArgs n [] fn of
        Just (args, [Ret ret]) -> res ss ss ss' args ret
        _ -> orig
  expr -> expr

  where
    collectArgs :: Int -> [Text] -> DartExpr -> Maybe ([Text], [DartExpr])
    collectArgs 1 acc (Lambda [oneArg] (Block js)) | length acc == n - 1
      = Just (reverse (oneArg : acc), js)
    collectArgs m acc (Lambda [oneArg] (Block [Ret ret]))
      = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _ _ = Nothing

isNFn :: Text -> Text -> Int -> DartExpr -> Bool
isNFn expectMod prefix n (Indexer _ (StringLiteral name) (VarRef modName))
  | modName == expectMod
    = name == fromString (T.unpack prefix <> show n)
isNFn _ _ _ _ = False

runFn :: Int -> DartExpr -> DartExpr
runFn = runFn' C.dataFunctionUncurried C.runFn FnDecl

--  FIXME: This doesn't work because it is not possible to return a named function.
runEffFn :: Text -> Text -> Int -> DartExpr -> DartExpr
runEffFn modName fnName = runFn' modName fnName $ \fn acc ->
  Lambda [] (Block [Ret (FnDecl fn acc)])

runFn' :: Text -> Text -> (DartExpr -> [DartExpr] -> DartExpr) -> Int -> DartExpr -> DartExpr
runFn' modName runFnName res n js = fromMaybe js $ go n [] js
  where
    go :: Int -> [DartExpr] -> DartExpr -> Maybe DartExpr
    go 0 acc (App ss runFnN [fn])
      | isNFn modName runFnName n runFnN && length acc == n
        = Just $ res ss fn acc
    go m acc (App _ lhs [arg])
      = go (m - 1) (arg : acc) lhs
    go _ _ _
      = Nothing

-}

semiringNumber :: forall a b. (IsString a, IsString b) => (a, b)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b) => (a, b)
semiringInt = (C.dataSemiring, C.semiringInt)

ringNumber :: forall a b. (IsString a, IsString b) => (a, b)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: forall a b. (IsString a, IsString b) => (a, b)
ringInt = (C.dataRing, C.ringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

eqNumber :: forall a b. (IsString a, IsString b) => (a, b)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: forall a b. (IsString a, IsString b) => (a, b)
eqInt = (C.dataEq, C.eqInt)

eqString :: forall a b. (IsString a, IsString b) => (a, b)
eqString = (C.dataEq, C.eqString)

eqChar :: forall a b. (IsString a, IsString b) => (a, b)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: forall a b. (IsString a, IsString b) => (a, b)
eqBoolean = (C.dataEq, C.eqBoolean)

ordBoolean :: forall a b. (IsString a, IsString b) => (a, b)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b) => (a, b)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: forall a b. (IsString a, IsString b) => (a, b)
ordInt = (C.dataOrd, C.ordInt)

ordString :: forall a b. (IsString a, IsString b) => (a, b)
ordString = (C.dataOrd, C.ordString)

ordChar :: forall a b. (IsString a, IsString b) => (a, b)
ordChar = (C.dataOrd, C.ordChar)

semigroupString :: forall a b. (IsString a, IsString b) => (a, b)
semigroupString = (C.dataSemigroup, C.semigroupString)

boundedBoolean :: forall a b. (IsString a, IsString b) => (a, b)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b) => (a, b)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: forall a b. (IsString a, IsString b) => (a, b)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

opAdd :: forall a b. (IsString a, IsString b) => (a, b)
opAdd = (C.dataSemiring, C.add)

opMul :: forall a b. (IsString a, IsString b) => (a, b)
opMul = (C.dataSemiring, C.mul)

opEq :: forall a b. (IsString a, IsString b) => (a, b)
opEq = (C.dataEq, C.eq)

opNotEq :: forall a b. (IsString a, IsString b) => (a, b)
opNotEq = (C.dataEq, C.notEq)

opLessThan :: forall a b. (IsString a, IsString b) => (a, b)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

opAppend :: forall a b. (IsString a, IsString b) => (a, b)
opAppend = (C.dataSemigroup, C.append)

opSub :: forall a b. (IsString a, IsString b) => (a, b)
opSub = (C.dataRing, C.sub)

opNegate :: forall a b. (IsString a, IsString b) => (a, b)
opNegate = (C.dataRing, C.negate)

opDiv :: forall a b. (IsString a, IsString b) => (a, b)
opDiv = (C.dataEuclideanRing, C.div)

opConj :: forall a b. (IsString a, IsString b) => (a, b)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: forall a b. (IsString a, IsString b) => (a, b)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: forall a b. (IsString a, IsString b) => (a, b)
opNot = (C.dataHeytingAlgebra, C.not)
