{-# LANGUAGE ViewPatterns #-}

module Language.PureScript.CodeGen.Dart.CoreImp
  ( fromModule
  ) where

-- import Debug.Trace

import Protolude (for, ordNub)

import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply (evalSupply)
import Control.Monad.Supply.Class

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Language.PureScript.Constants as C
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString)
import Language.PureScript.Traversals (sndM)

-- import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer (optimize)
import Language.PureScript.CodeGen.Dart.Command.Options as Dart
import Language.PureScript.CodeGen.Dart.Ident
import qualified Language.PureScript.CodeGen.Dart.CoreImp.AST as D
import Language.PureScript.CodeGen.Dart.CoreImp.AST (DartExpr)
import Language.PureScript.CodeGen.Dart.CoreImp.Directives

import System.FilePath.Posix ((</>))

fromModule :: Dart.CommandLineOptions -> String -> String -> Module Ann -> [DartExpr]
fromModule
  CommandLineOptions{..}
  packageName
  libraryPrefix
  (Module _ comments mn _ imports exports foreigns bindings)
  = evalSupply 0 $ do
    let
      foreignDecls =
        if null foreigns
          then []
          else getForeignDirectives packageName libraryPrefix mn
      importDecls =
        getImportDirectives packageName libraryPrefix mn (snd <$> imports)
    optDecls <- fromDecls mn True bindings
    return $ concat [foreignDecls, importDecls, optDecls]

-- | CoreFn to Dart IR
fromDecls :: forall m . (Monad m, MonadSupply m) => ModuleName -> Bool ->  [Bind Ann] -> m [DartExpr]
fromDecls mn cloStripComments bindings = do
  rawDecls <- mapM fromBinding bindings
  -- TODO: Optimize
  let optDecls = rawDecls
  return $ concat optDecls

  where

  fromBinding :: Bind Ann -> m [DartExpr]
  fromBinding = \case
    NonRec _ ident expr -> return <$> fromNonRec ident expr
    Rec bindings -> forM bindings $ \((_, ident), expr) -> fromNonRec ident expr

  fromNonRec :: Ident -> Expr Ann -> m DartExpr
  -- Commented declaration
  fromNonRec i = \case
    -- Comment
    e@(extractAnn -> (_, com, _, _)) | not (null com) ->
      case cloStripComments of
        True -> fromNonRec i uncommented
        False -> D.Comment com <$> fromNonRec i uncommented
      where
        uncommented = modifyAnn removeComments e

  -- Typeclass declaration
    e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) ->
      return $ D.ClassDecl (fromIdent i) (map fromIdent (unAbs e))
      where
        unAbs :: Expr Ann -> [Ident]
        unAbs (Abs _ arg val) = arg : unAbs val
        unAbs _ = []

  --  Function declaration
  --  FIXME: Eliminate hacky carveout to avoid application to typeclass instance methods
  --  TODO: Add prefer-inline annotation to top-level bindings for typeclass instance methods
    (Abs _ arg val) | arg /= Ident "dict" -> do
      ret <- fromExpr val
      return $ D.FnDecl
        (Just $ fromIdent i)
        --  FIXME: Is this consistent?  Why do some function translations eliminate the unused identifier while others retain it? Unused identifiers sometimes make it through CoreFn.
        (case arg of
          UnusedIdent -> []
          _           -> [fromIdent arg]
        )
        (case ret of
          D.Block _ -> ret
          _ -> D.Block [D.Return (Just ret)]
        )

    e -> fromExpr e >>= \case
      decl@(D.ClassDecl{}) -> return decl
      expr -> return $ D.VarDecl (fromIdent i) expr

  fromExpr :: Expr Ann -> m DartExpr
  fromExpr = \case

    Literal _ l ->
      fromLiteral l

    Var (_, _, _, Just (IsConstructor _ [])) name ->
      return $ D.FnCall (fromVar name) []

    Var (_, _, _, Just (IsConstructor _ _)) name ->
      return $ D.ObjectAccessor "create" (fromVar name)

    Abs (_, _, _, Just IsTypeClassConstructor) _ ctor ->
      internalError $ "Encountered a type class constructor not processed as a top-level binding:" <> show ctor

    Abs _ (Ident "dict") (Accessor _ prop val@(Var _ (Qualified Nothing (Ident "dict")))) -> do
      let field = fromMaybe (internalError $ "Encountered a typeclass method name that was not a decodable string:" <> show prop) (decodeString prop)
      body <- D.ObjectAccessor (DartIdent field) <$> fromExpr val
      return $ D.Lambda
        ["dict"]
        (D.Block [D.Return (Just body)])

    -- Anonymous function declaration
    Abs _ arg val -> do
      ret <- fromExpr val
      return $
        D.Lambda
          (case arg of
            UnusedIdent -> []
            _           -> [fromIdent arg]
          )
          (case ret of
            D.Block _ -> ret
            _ -> D.Block [D.Return (Just ret)]
          )

    Accessor _ prop val ->
      D.RecordAccessor prop <$> fromExpr val

    -- Shallow copy an open record that has not already been desugared to a
    -- record literal (as generally will occur for closed record updates).
    -- Map.from(obj).addAll(kvs)
    ObjectUpdate _ o ps -> do
      obj <- fromExpr o
      kvs <- mapM (sndM fromExpr) ps
      return $
        D.MethodCall
          (D.FnCall (D.VarRef "Map.from") [obj])
          "addAll"
          [ D.RecordLiteral kvs ]

    e@App{} -> do
      let (f, as) = collectFnArgs e []
      args <- mapM fromExpr as
      case f of
        --  If the function call is to a newtype constructor, inline the call with the value that is already there.
        Var (_, _, _, Just IsNewtype) _ -> case args of
          [inner] -> return inner
          _ -> internalError $ "Encountered newtype constructor call (" <> show f <> ") with more than one argument: " <> show args
        --  If the function call is fully saturated, do not curry the call; invoke the generated constructor with all its arguments directly.
        Var (_, _, _, Just (IsConstructor _ fields)) name
          | length args == length fields ->
              return $ D.FnCall (fromVar name) args
        --  If the function call constructs a typeclass dictionary, it will always be fully saturated.
        Var (_, _, _, Just IsTypeClassConstructor) name ->
          return $ D.FnCall (fromVar name) args
        --  Otherwise, for a generic function, apply the call in curried fashion
        _ -> flip (foldl (\fn a -> D.FnCall fn [a])) args <$> fromExpr f
      where
        collectFnArgs :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        collectFnArgs (App _ val arg) args = collectFnArgs val (arg : args)
        collectFnArgs other args = (other, args)

    Var (_, _, _, Just IsForeign) ref -> case ref of
      qualified@(Qualified (Just mn') ident)
        | mn' == mn -> return $ fromForeign ident
        | otherwise -> return $ fromVar qualified
      unqualified ->
        internalError $ "Encountered an unqualified reference to a foreign ident " <> T.unpack (showQualified showIdent unqualified)

    Var _ ident -> return $ fromVar ident

    Case _ values binders -> do
      vals <- mapM fromExpr values
      fromCases binders vals

    -- NOTE: Dart syntax requires the IIFE because it prohibits
    -- returning an anonymous block containing a return value.
    Let _ bindings expr -> do
      decls <- concat <$> mapM fromBinding bindings
      ret <- fromExpr expr
      return $ D.IIFE (decls ++ [D.Return (Just ret)])

    -- Newtype constructor
    -- TODO: Figure out why the IsNewtype case gets eliminated.
    -- The translations of `Var` and `App` do not seem to allow this ever to
    -- be called even if it were triggered so as to generate code.
    -- If it were, there should be a class declaration with a static create
    -- method that has the semantics of the identity function.
    --  See https://github.com/purescript/purescript/blob/2963edd9e9c02b7284f1809ff09b62f4a8c1b128/src/Language/PureScript/CodeGen/JS.hs#L249
    Constructor (_, _, _, ctorType) _ ctor fields
      | ctorType == Just IsNewtype ->
          internalError . T.unpack $ "Encountered newtype constructor as record literal rather than as function declaration" <> runProperName ctor
      | otherwise -> return $
          D.ClassDecl (fromProperName ctor) (fromIdent <$> fields)

  fromLiteral :: Literal (Expr Ann) -> m DartExpr
  fromLiteral = \case
    NumericLiteral (Left i) -> return $ D.IntegerLiteral i
    NumericLiteral (Right n) -> return $ D.DoubleLiteral n
    StringLiteral s -> return $ D.StringLiteral s
    CharLiteral c -> return $ D.StringLiteral (fromString [c])
    BooleanLiteral b -> return $ D.BooleanLiteral b
    ArrayLiteral xs -> D.ArrayLiteral <$> mapM fromExpr xs
    ObjectLiteral ps -> D.RecordLiteral <$> mapM (sndM fromExpr) ps

  fromVar :: Qualified Ident -> DartExpr
  fromVar (Qualified Nothing i) = D.VarRef (fromIdent i)
  fromVar qualified = fromQualified fromIdent qualified

  fromQualified :: (a -> DartIdent) -> Qualified a -> DartExpr
  fromQualified f (Qualified q a) = case q of
    Just (ModuleName [ProperName mn']) | mn' == C.prim ->
      D.VarRef (f a)
    Just mn' | mn /= mn' ->
      D.ObjectAccessor (f a) (D.VarRef (fromModuleName mn'))
    _ -> D.VarRef (f a)

  fromForeign :: Ident -> DartExpr
  fromForeign i =
    D.ObjectAccessor (fromIdent i) (D.VarRef "$foreign")

  --  NOTE: This generates unused variable bindings and may insert an exhaustivity check (throw) even in cases where it would be dead code.
  --  The implementation relies on an optimization pass and/or the Dart analyzer to remove these.
  fromCases :: [CaseAlternative Ann] -> [DartExpr] -> m DartExpr
  fromCases binders vals = do
    valNames <- map fromAnyName <$> replicateM (length vals) freshName
    let assignments = zipWith D.VarDecl valNames vals
    imps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- fromGuards result
      go valNames ret bs
    return $ D.IIFE
      ( assignments
      ++ concat imps
      ++ [D.Throw (D.FnCall (D.VarRef "FallThroughError") [])]
      )

    where
      go :: [DartIdent] -> [DartExpr] -> [Binder Ann] -> m [DartExpr]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        fromCaseBinder (D.VarRef v) done'' b
      go _ _ _ =
        internalError $ "Encountered invalid arguments when converting case binders (" <> show binders <> ") and values (" <> show vals <> ")."

      fromGuards :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [DartExpr]
      fromGuards = \case
        Left gs -> for gs $ \(cond, val) -> do
          cond' <- fromExpr cond
          val' <- fromExpr val
          return $ D.IfThen cond' (D.Block [D.Return (Just val')])
        Right v -> return . D.Return . Just <$> fromExpr v

  fromCaseBinder :: DartExpr -> [DartExpr] -> Binder Ann -> m [DartExpr]
  fromCaseBinder varRef done = \case
    NullBinder{} -> return done
    LiteralBinder _ lit ->
      fromLiteralBinder varRef done lit
    VarBinder _ i ->
      return (D.VarDecl (fromIdent i) varRef : done)
    ConstructorBinder (_, _, _, meta) _ ctor bs -> case (meta, bs) of
      (Just IsNewtype, [b]) -> fromCaseBinder varRef done b
      (Just (IsConstructor ctorType fs), _) -> do
        imp <- go (zip fs bs) done
        return $ case ctorType of
          ProductType -> imp
          SumType ->
            [ D.IfThen
              (D.Binary D.Is varRef (fromQualified fromProperName ctor))
              (D.Block imp)
            ]
        where
          go :: [(Ident, Binder Ann)] -> [DartExpr] -> m [DartExpr]
          go [] done' = return done'
          go ((field, binder) : remain) done' = do
            argVar <- fromAnyName <$> freshName
            done'' <- go remain done'
            imp <- fromCaseBinder (D.VarRef argVar) done'' binder
            return (D.VarDecl argVar (D.ObjectAccessor (fromIdent field) varRef) : imp)
      _ -> internalError $ "Encountered invalid constructor binder:" <> show ctor
    NamedBinder _ ident binder -> do
      imp <- fromCaseBinder varRef done binder
      return (D.VarDecl (fromIdent ident) varRef : imp)

  fromLiteralBinder :: DartExpr -> [DartExpr] -> Literal (Binder Ann) -> m [DartExpr]
  fromLiteralBinder varRef done = \case
    NumericLiteral (Left i) -> return . pure $
      D.IfEqual varRef (D.IntegerLiteral i) (D.Block done)
    NumericLiteral (Right n) -> return . pure $
      D.IfEqual varRef (D.DoubleLiteral n) (D.Block done)
    CharLiteral c -> return . pure $
      D.IfEqual varRef (D.StringLiteral (fromString [c])) (D.Block done)
    StringLiteral str -> return . pure $
      D.IfEqual varRef (D.StringLiteral str) (D.Block done)
    BooleanLiteral True -> return . pure $
      D.IfThen varRef (D.Block done)
    BooleanLiteral False -> return . pure $
      D.IfThen (D.Unary D.Not varRef) (D.Block done)
    ObjectLiteral bs -> go done bs
      where
        go :: [DartExpr] -> [(PSString, Binder Ann)] -> m [DartExpr]
        go done' [] = return done'
        go done' ((prop, binder):bs') = do
          propVar <- fromAnyName <$> freshName
          done'' <- go done' bs'
          imp <- fromCaseBinder (D.VarRef propVar) done'' binder
          return (D.VarDecl propVar (D.RecordAccessor prop varRef) : imp)
    ArrayLiteral bs -> do
      imp <- go done 0 bs
      return . pure $
        D.IfEqual
          (D.ObjectAccessor "length" varRef)
          (D.IntegerLiteral (fromIntegral $ length bs))
          (D.Block imp) -- FIXME
      where
        go :: [DartExpr] -> Integer -> [Binder Ann] -> m [DartExpr]
        go done' _ [] = return done'
        go done' index (binder:bs') = do
          elVar <- fromAnyName <$> freshName
          done'' <- go done' (index + 1) bs'
          imp <- fromCaseBinder (D.VarRef elVar) done'' binder
          return $ D.VarDecl elVar (D.ArrayAccessor index varRef) : imp
