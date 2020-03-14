{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module generates code in the core imperative representation from
-- elaborated PureScript code.
module Language.PureScript.CodeGen.Dart.CoreImp
  ( module AST
  , module Common
  , moduleToJs
  ) where

-- import Debug.Trace

import Protolude (ordNub)

import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Data.Aeson.Casing (snakeCase)
import Data.Foldable (foldl')
import Data.List ((\\))
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import qualified Language.PureScript.Constants as C
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow, errorMessage,
                                   errorMessage', rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Traversals (sndM)

-- FIXME
-- import Language.PureScript.CoreImp.Optimizer

import Language.PureScript.CodeGen.Dart.Options as Dart
import Language.PureScript.CodeGen.Dart.Common as Common
import qualified Language.PureScript.CodeGen.Dart.CoreImp.AST as AST
import Language.PureScript.CodeGen.Dart.CoreImp.AST (AST, everywhereTopDownM, withSourceSpan)

import System.FilePath.Posix ((</>))

-- | Generate code in the simplified JavaScript intermediate representation for all declarations in a
-- module.
moduleToJs
  :: forall m
   . (Monad m, MonadReader Dart.Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe Text -- ^ Foreign includes, if any
  -> m [AST]
moduleToJs (Module _ _ {- comments -} mn _ imports _ {- exports -} foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imports
    let decls' = renameModules mnLookup decls
    astDecls <- mapM bindToAst decls'
    -- FIXME
    -- optimized <- traverse (traverse optimize) astDecls
    let optimized = astDecls
    let mnReverseLookup = M.fromList $ map (\(origName, (_, safeName)) -> (moduleNameToJs safeName, origName)) $ M.toList mnLookup
    let usedModuleNames = foldMap (foldMap (findModules mnReverseLookup)) optimized
    jsImports <- traverse (importToJs mnLookup)
      . filter (flip S.member usedModuleNames)
      . (\\ (mn : C.primModules)) $ ordNub $ map snd imports
    F.traverse_ (F.traverse_ checkIntegers) optimized
    --  Remove the use strict annotation and the foreign imports, and replace with a library and foreign import/re-export statement or another suitable scheme. Probably importing a child library is the easiest given namespacing restrictions.  Parallel foreign module with its own library directive.
    {-
    -- comments <- not <$> asks optionsNoComments
    let library = AST.Directive Nothing (AST.Library "index")
    let header =
          if comments && not (null coms)
            then AST.Comment Nothing coms library
            else library
    -}
    -- foreigns represents an AST
    let foreign' = case foreign_ of
          Just ffi | not (null foreigns) ->
            [ AST.Directive Nothing (AST.Import ffi "$foreign")
            , AST.Directive Nothing (AST.Export ffi [])
            ]
          _ ->
            []
    return $ {- header : -} foreign' ++ jsImports ++ concat optimized
    {- Rename these with underscores
    let foreignExps = exps `intersect` foreigns
    let standardExps = exps \\ foreignExps
    let exps' = AST.RecordLiteral Nothing $ map (mkString . runIdent &&& AST.Var Nothing . identToJs) standardExps
                               ++ map (mkString . runIdent &&& foreignIdent) foreignExps
    -}

  where

  -- | Extracts all declaration names from a binding group.
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec _ ident _) = [ident]
  getNames (Rec vals) = map (snd . fst) vals

  -- | Creates alternative names for each module to ensure they don't collide
  -- with declaration names.
  renameImports :: [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
  renameImports = go M.empty
    where
    go :: M.Map ModuleName (Ann, ModuleName) -> [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
    go acc used ((ann, mn') : mns') =
      let mni = Ident $ runModuleName mn'
      in if mn' /= mn && mni `elem` used
         then let newName = freshModuleName 1 mn' used
              in go (M.insert mn' (ann, newName) acc) (Ident (runModuleName newName) : used) mns'
         else go (M.insert mn' (ann, mn') acc) used mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
    freshModuleName i mn'@(ModuleName pns) used =
      let newName = ModuleName $ init pns ++ [ProperName $ runProperName (last pns) <> "_" <> T.pack (show i)]
      in if Ident (runModuleName newName) `elem` used
         then freshModuleName (i + 1) mn' used
         else newName

  -- | Generates Dart code for a module import, binding the required module to the alternative
  importToJs :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m AST
  importToJs mnLookup mn' = do
    let
      ((ss, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      dartModulePath = foldl' (</>) "" $ snakeCase . T.unpack <$> T.split (=='.') (runModuleName mn')
    withPos ss $ AST.Directive Nothing
      (AST.Import
        (fromString ("package:pkg" </> dartModulePath </> "index.dart"))
        (moduleNameToJs mnSafe)
      )

  -- | Replaces the `ModuleName`s in the AST so that the generated code refers to the collision-avoiding renamed module imports.
  renameModules :: M.Map ModuleName (Ann, ModuleName) -> [Bind Ann] -> [Bind Ann]
  renameModules mnLookup binds =
    let (f, _, _) = everywhereOnValues id goExpr goBinder
    in map f binds
    where
    goExpr :: Expr a -> Expr a
    goExpr (Var ann q) = Var ann (renameQual q)
    goExpr e = e
    goBinder :: Binder a -> Binder a
    goBinder (ConstructorBinder ann q1 q2 bs) =
      ConstructorBinder ann (renameQual q1) (renameQual q2) bs
    goBinder b = b
    renameQual :: Qualified a -> Qualified a
    renameQual (Qualified (Just mn') a) =
      let (_,mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      in Qualified (Just mnSafe) a
    renameQual q = q

  -- |
  -- Find the set of ModuleNames referenced by an AST.
  --
  findModules :: M.Map Text ModuleName -> AST -> S.Set ModuleName
  findModules mnReverseLookup = AST.everything mappend go
    where
    go (AST.Var _ name) = foldMap S.singleton $ M.lookup name mnReverseLookup
    go _ = mempty

  -- |
  -- Generate code in the simplified JavaScript intermediate representation for a declaration
  --
  bindToAst :: Bind Ann -> m [AST]
  bindToAst (NonRec ann ident val) = return <$> nonRecToJS ann ident val
  bindToAst (Rec vals) = forM vals (uncurry . uncurry $ nonRecToJS)

  -- | Generate code in the simplified JavaScript intermediate representation for a single non-recursive declaration.
  --
  nonRecToJS :: Ann -> Ident -> Expr Ann -> m AST
  nonRecToJS a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToJS a i (modifyAnn removeComments e)
       else AST.Comment Nothing com <$> nonRecToJS a i (modifyAnn removeComments e)
  -- Dart name collisions possible if typeclasses and data constructors have
  -- different namespaces, but that would also be the case for JavaScript
  -- where both are functions.
  -- TODO: Typeclass renderings don't need the curried "create" function.
  nonRecToJS (ss, _, _, _) ident e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) = do
    let args = unAbs e
    return $ AST.ClassDeclaration (Just ss)
      (AST.ConcreteClass $ identToJs ident)
      []
      (map identToJs args)
    where
    unAbs :: Expr Ann -> [Ident]
    unAbs (Abs _ arg val) = arg : unAbs val
    unAbs _ = []
  nonRecToJS _ ident@(Ident i) (Abs _ arg val) | i /= "dict" = do
    ret <- valueToJs val
    let jsArg = case arg of
          UnusedIdent -> []
          _           -> [identToJs arg]
    return $
      AST.Function Nothing
        (Just $ identToJs ident)
        jsArg
        (case ret of
          AST.Block _ _ -> ret
          _ -> AST.Block Nothing [AST.Return Nothing ret]
        )
  nonRecToJS (ss, _, _, _) ident val = do
    imp <- valueToJs val
    withPos ss $ case imp of
      decl@(AST.ClassDeclaration _ _ _ _) -> decl
      _ -> AST.VariableIntroduction Nothing (identToJs ident) (Just imp)

  withPos :: SourceSpan -> AST -> m AST
  withPos _ imp = return imp

  -- | Generate code in the simplified JavaScript intermediate representation for a variable based on a
  -- PureScript identifier.
  var :: Ident -> AST
  var = AST.Var Nothing . identToJs

  -- | Generate code in the simplified JavaScript intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in JavaScript (symbol based, reserved name) an
  -- indexer is returned.
--  recordAccessor :: Ident -> AST -> AST
--  recordAccessor (Ident prop) = recordAccessorString $ mkString prop
--  recordAccessor (GenIdent _ _) = internalError "GenIdent in accessor"
--  recordAccessor UnusedIdent = internalError "UnusedIdent in accessor"

  -- | Generate a record access
  recordAccessorString :: PSString -> AST -> AST
  recordAccessorString prop =
    AST.RecordAccessor Nothing (AST.StringLiteral Nothing prop)

  objectAccessor :: Ident -> AST -> AST
  objectAccessor (Ident prop) = objectAccessorString $ mkString prop
  objectAccessor (GenIdent _ _) = internalError "GenIdent in accessor"
  objectAccessor UnusedIdent = internalError "UnusedIdent in accessor"

  -- | Generate a record access
  objectAccessorString :: PSString -> AST -> AST
  objectAccessorString prop =
    AST.ObjectAccessor Nothing (AST.StringLiteral Nothing prop)

  -- | Generate code in the simplified JavaScript intermediate representation for a value or expression.
  valueToJs :: Expr Ann -> m AST
  valueToJs e =
    let (ss, _, _, _) = extractAnn e in
    withPos ss =<< valueToJs' e

  valueToJs' :: Expr Ann -> m AST
  valueToJs' (Literal (pos, _, _, _) l) =
    rethrowWithPosition pos $ literalToValueJS pos l
  valueToJs' (Var (_, _, _, Just (IsConstructor _ [])) name) =
    return $ AST.App Nothing (qualifiedToJS id name) []
  valueToJs' (Var (_, _, _, Just (IsConstructor _ _)) name) =
    return $ objectAccessorString "create" $ qualifiedToJS id name
  -- internalError if IsTypeClassConstructor -- should have been intercepted

  -- FIXME: Does this discriminate sufficiently between records and objects?
  -- Prime case may be typeclass instance method invocations.
  -- This could be fixed by adding a meta annotation to the Accessor
  -- or to the Abs
  {-
    -- https://github.com/purescript/purescript/blob/ed5fbfb75eb7d85431591d0c889fa8ada7174fd6/src/Language/PureScript/CoreFn/Desugar.hs#L115
    exprToCoreFn ss com ty  (A.TypeClassDictionaryAccessor _ ident) =
        Abs (ss, com, ty, Nothing)
          (Ident "dict")
          (Accessor (ssAnn ss)
            (mkString $ runIdent ident)
            (Var (ssAnn ss) $ Qualified Nothing (Ident "dict"))
          )
  -}
  valueToJs' (Abs _ (Ident "dict") (Accessor _ prop val@(Var _ (Qualified Nothing (Ident "dict"))))) = do
    body <- objectAccessorString prop <$> valueToJs val
    return $ AST.Function Nothing
      Nothing
      ["dict"]
      (AST.Block Nothing [AST.Return Nothing body])
  valueToJs' (Abs _ arg val) = do
    ret <- valueToJs val
    let jsArg = case arg of
          UnusedIdent -> []
          _           -> [identToJs arg]
    return $
      AST.Function Nothing
        Nothing
        jsArg
        (case ret of
          AST.Block _ _ -> ret
          _ -> AST.Block Nothing [AST.Return Nothing ret]
        )
  valueToJs' (Accessor _ prop val) =
    recordAccessorString prop <$> valueToJs val
  valueToJs' (ObjectUpdate _ o ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    extendObj obj sts
  valueToJs' e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      --  If the function call is to a newtype constructor, inline the call with the value that is already there.
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      --  If the function call is saturated, do not curry the call; invoke the generated constructor with all its arguments directly.
      Var (_, _, _, Just (IsConstructor _ fields)) name
        | length args == length fields ->
            return $ AST.App Nothing (qualifiedToJS id name) args'
      --  If the function call constructs a typeclass dictionary, it will always be fully saturated.
      Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ AST.App Nothing (qualifiedToJS id name) args'
      --  Otherwise, for a generic function, apply the call in curried fashion
      _ -> flip (foldl (\fn a -> AST.App Nothing fn [a])) args' <$> valueToJs f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToJs' (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs qi
  valueToJs' (Var (_, _, _, Just IsForeign) ident) =
    internalError $ "Encountered an unqualified reference to a foreign ident " ++ T.unpack (showQualified showIdent ident)
  valueToJs' (Var _ ident) = return $ varToJs ident
  valueToJs' (Case (ss, _, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs ss binders vals
  valueToJs' (Let _ ds val) = do
    ds' <- concat <$> mapM bindToAst ds
    ret <- valueToJs val
    return $
      AST.App Nothing
        (AST.Function Nothing
          Nothing
          []
          (AST.Block Nothing (ds' ++ [AST.Return Nothing ret])))
        []
--    NOTE: The simpler version does not comply with Dart syntax, as
--    it is not permitted to return a block { } with statements and a return.
--    So there is an immediately invoked function.  In principle this code
--    could be reorganized.
--    return $ AST.Block Nothing (ds' ++ [AST.Return Nothing ret])

  -- Newtype constructor
  -- TODO: Figure out why this gets eliminated.
  valueToJs' (Constructor (_, _, _, Just IsNewtype) _ ctor _) = return $
    AST.VariableIntroduction Nothing
      (properToJs ctor)
      (Just $
        AST.RecordLiteral Nothing
        [ ("schmeate",  AST.Function Nothing
                        Nothing
                        ["value"]
                        (AST.Block Nothing
                          [AST.Return Nothing $ AST.Var Nothing "value"]
                        )
        ) ]
      )

  -- Nullary constructor
  valueToJs' (Constructor _ _ ctor []) = return $
    AST.ClassDeclaration Nothing (AST.ConcreteClass $ properToJs ctor) [] []

  -- N-ary constructor
  valueToJs' (Constructor _ _ ctor fields) = return $
    AST.ClassDeclaration Nothing
      (AST.ConcreteClass $ properToJs ctor)
      []
      (identToJs <$> fields)

  literalToValueJS :: SourceSpan -> Literal (Expr Ann) -> m AST
  literalToValueJS ss (NumericLiteral (Left i)) = return $ AST.IntegerLiteral (Just ss) i
  literalToValueJS ss (NumericLiteral (Right n)) = return $ AST.DoubleLiteral (Just ss) n
  literalToValueJS ss (StringLiteral s) = return $ AST.StringLiteral (Just ss) s
  literalToValueJS ss (CharLiteral c) = return $ AST.StringLiteral (Just ss) (fromString [c])
  literalToValueJS ss (BooleanLiteral b) = return $ AST.BooleanLiteral (Just ss) b
  literalToValueJS ss (ArrayLiteral xs) = AST.ArrayLiteral (Just ss) <$> mapM valueToJs xs
  literalToValueJS ss (ObjectLiteral ps) = AST.RecordLiteral (Just ss) <$> mapM (sndM valueToJs) ps

  -- | Shallow copy an open record in an object update.
  extendObj :: AST -> [(PSString, AST)] -> m AST
  extendObj obj sts = do
    return $
      AST.App Nothing
        (objectAccessorString "addAll" $
          (AST.App Nothing
            (AST.Var Nothing "Map.from")
            [obj]
          )
        )
        [ AST.RecordLiteral Nothing sts ]

  -- | Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable.
  varToJs :: Qualified Ident -> AST
  varToJs (Qualified Nothing ident) = var ident
  varToJs qual = qualifiedToJS id qual

  -- | Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  qualifiedToJS :: (a -> Ident) -> Qualified a -> AST
  qualifiedToJS f (Qualified (Just (ModuleName [ProperName mn'])) a)
    | mn' == C.prim = AST.Var Nothing . identToJs $ f a
  qualifiedToJS f (Qualified (Just mn') a)
    | mn /= mn' =
        objectAccessor (f a) (AST.Var Nothing (moduleNameToJs mn'))
  qualifiedToJS f (Qualified _ a) = AST.Var Nothing $ identToJs (f a)

  foreignIdent :: Ident -> AST
  foreignIdent ident = objectAccessorString (mkString $ identToJs ident) (AST.Var Nothing "$foreign")

  -- | Generate code in the simplified JavaScript intermediate representation for pattern match binders and guards.
  --  FIXME: This generates assignment bindings for unused variables in Dart
  --  when unused variable bindings are generated in PureScript.  Additionally
  --  this binds unused variables when NO unused variables are ggenerated in
  --  PureScript.
  bindersToJs :: SourceSpan -> [CaseAlternative Ann] -> [AST] -> m AST
  bindersToJs _ binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith (AST.VariableIntroduction Nothing) valNames (map Just vals)
    imps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToJs result
      go valNames ret bs

    --  NOTE:
    --  Some blocks have no return statement, since they are all in conditionals, and so need to throw a FallThroughError() as a bottom case (at least as it appears to the Dart compiler).
    --  But other blocks necessarily have a return statement, because they catch all cases, and in that case throwing a FallThroughError() is unreachable dead code.
    --  So exhaustivity checks would need to be repeated, at least to the extent of eliminating an AST.Throw if the immediately preceding statement is a standalone AST.Return.
    --  Another data structure would be more efficient
    let (stmts, exhaustiveCheck) = case concat imps of
          [] -> ([], throwing)
          stmts' | AST.Return _ _ <- last stmts' -> (stmts', [])
          stmts' -> (stmts', throwing)
        throwing = [AST.Throw Nothing fallthroughError]
    return $ AST.App Nothing
      (AST.Function Nothing
        Nothing
        []
        (AST.Block Nothing
          ( assignments
          ++ stmts
          ++ exhaustiveCheck
          )
        )
      )
      []

    where
      go :: [Text] -> [AST] -> [Binder Ann] -> m [AST]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToJs v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToJs"

      fallthroughError :: AST
      fallthroughError =
        AST.App Nothing
          (AST.Var Nothing "FallThroughError")
          []

      guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [AST]
      guardsToJs (Left gs) = traverse genGuard gs where
        genGuard (cond, val) = do
          cond' <- valueToJs cond
          val'   <- valueToJs val
          return
            (AST.IfElse Nothing cond'
              (AST.Block Nothing [AST.Return Nothing val']) Nothing)

      guardsToJs (Right v) = return . AST.Return Nothing <$> valueToJs v

  binderToJs :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs s done binder =
    let (ss, _, _, _) = extractBinderAnn binder in
    traverse (withPos ss) =<< binderToJs' s done binder

  -- | Generate code in the simplified JavaScript intermediate representation for a pattern match
  -- binder.
  binderToJs' :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs' _ done NullBinder{} = return done
  binderToJs' varName done (LiteralBinder _ l) =
    literalToBinderJS varName done l
  binderToJs' varName done (VarBinder _ ident) =
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (AST.Var Nothing varName)) : done)
  binderToJs' varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs' varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    imp <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> imp
      SumType ->
        [AST.IfElse Nothing (AST.Is Nothing (AST.Var Nothing varName) (qualifiedToJS (Ident . runProperName) ctor))
                  (AST.Block Nothing imp)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [AST] -> m [AST]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      imp <- binderToJs argVar done'' binder
      return (AST.VariableIntroduction Nothing argVar (Just $ objectAccessorString (mkString $ identToJs field) $ AST.Var Nothing varName) : imp)
  binderToJs' _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs' varName done (NamedBinder _ ident binder) = do
    imp <- binderToJs varName done binder
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (AST.Var Nothing varName)) : imp)

  literalToBinderJS :: Text -> [AST] -> Literal (Binder Ann) -> m [AST]
  literalToBinderJS varName done (NumericLiteral num) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.NumericLiteral Nothing num)) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (CharLiteral c) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing (fromString [c]))) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (StringLiteral str) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing str)) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (BooleanLiteral True) =
    return [AST.IfElse Nothing (AST.Var Nothing varName) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (BooleanLiteral False) =
    return [AST.IfElse Nothing (AST.Unary Nothing AST.Not (AST.Var Nothing varName)) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (ObjectLiteral bs) = go done bs
    where
    go :: [AST] -> [(PSString, Binder Ann)] -> m [AST]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      imp <- binderToJs propVar done'' binder
      return (AST.VariableIntroduction Nothing propVar (Just (recordAccessorString prop (AST.Var Nothing varName))) : imp)
  literalToBinderJS varName done (ArrayLiteral bs) = do
    imp <- go done 0 bs
    return
      [ AST.IfElse Nothing
          (AST.Binary Nothing AST.EqualTo (objectAccessorString "length" (AST.Var Nothing varName))
          (AST.NumericLiteral Nothing (Left (fromIntegral $ length bs))))
          (AST.Block Nothing imp) Nothing
      ]
    where
    go :: [AST] -> Integer -> [Binder Ann] -> m [AST]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      imp <- binderToJs elVar done'' binder
      let ev = AST.VariableIntroduction Nothing
                elVar
                (Just (AST.ArrayIndexer Nothing
                  (AST.IntegerLiteral Nothing index)
                  (AST.Var Nothing varName)))
      return $ ev : imp

  -- Check that all integers fall within the valid int range for JavaScript.
  checkIntegers :: AST -> m ()
  checkIntegers = void . everywhereTopDownM go
    where
    go :: AST -> m AST
    go (AST.Unary _ AST.Negate (AST.NumericLiteral ss (Left i))) =
      -- Move the negation inside the literal; since this is a top-down
      -- traversal doing this replacement will stop the next case from raising
      -- the error when attempting to use -2147483648, as if left unrewritten
      -- the value is `Unary Negate (NumericLiteral (Left 2147483648))`, and
      -- 2147483648 is larger than the maximum allowed int.
      return $ AST.NumericLiteral ss (Left (-i))
    go imp@(AST.NumericLiteral ss (Left i)) =
      let minInt = -2147483648
          maxInt = 2147483647
      in if i < minInt || i > maxInt
         then throwError . maybe errorMessage errorMessage' ss $ IntOutOfRange i "JavaScript" minInt maxInt
         else return imp
    go other = return other
