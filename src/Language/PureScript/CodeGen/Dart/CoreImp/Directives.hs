module Language.PureScript.CodeGen.Dart.CoreImp.Directives where

import Protolude (ordNub)

import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply (evalSupply)
import Control.Monad.Supply.Class

import Data.Foldable (foldl')
import Data.List ((\\))
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Language.PureScript.Constants as C
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow, errorMessage,
                                   errorMessage', rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.Traversals (sndM)

import Language.PureScript.CodeGen.Dart.CoreImp.Optimizer (optimize)
import Language.PureScript.CodeGen.Dart.Command.Options as Dart
import Language.PureScript.CodeGen.Dart.Common as Common
import qualified Language.PureScript.CodeGen.Dart.CoreImp.AST as AST
import Language.PureScript.CodeGen.Dart.CoreImp.AST (AST, everywhereTopDownM, withSourceSpan)

import System.FilePath.Posix ((</>))


  let (renamedDecls, mnLookup, mnReverseLookup) = renameDecls

  -- Rename variables to avoid collisions with module names
  let usedNames = concatMap getNames decls
  let mnLookup = renameImports usedNames imports
  let decls' = renameModules mnLookup decls

  let mnReverseLookup = M.fromList $ map (\(origName, (_, safeName)) -> (moduleNameToJs safeName, origName)) $ M.toList mnLookup

  let usedModuleNames = foldMap (foldMap (findModules mnReverseLookup)) optimized

getUsedNames :: [AST] -> [AST]

getUsedModuleNames :: [AST] -> [AST]

getForeignDirectives :: [AST]

  -- Import used modules
  let usedModuleNames = foldMap (foldMap (findModules mnReverseLookup)) optimized

  importDecls <- traverse (importToDart mnLookup)
    . filter (flip S.member usedModuleNames)
    . (\\ (mn : C.primModules)) $ ordNub $ map snd imports

  -- Import foreign library, if any
  let importExportDecls = case null foreigns of
        True -> importDecls
        False ->
          [ AST.Directive Nothing (AST.Import filename "$foreign")
          , AST.Directive Nothing (AST.Export filename [])
          ] ++ importDecls
          where
            moduleName = T.unpack $ runModuleName mn
            filename = T.pack $ "package:" <>
              (toTargetImportName packageName libraryPrefix "foreign" moduleName)

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
  importToJs :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m DartExpr
  importToJs mnLookup mn' = do
    let
      ((ss, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      moduleName = T.unpack (runModuleName mn')
    withPos ss $ DartExpr.Directive Nothing
      (DartExpr.Import
        (fromString ("package:" <> toTargetImportName packageName libraryPrefix "index" moduleName))
        (moduleNameToJs mnSafe)
      )

  -- | Replaces the `ModuleName`s in the DartExpr so that the generated code refers to the collision-avoiding renamed module imports.
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
  -- Find the set of ModuleNames referenced by an DartExpr.
  --
  findModules :: M.Map Text ModuleName -> DartExpr -> S.Set ModuleName
  findModules mnReverseLookup = DartExpr.everything mappend go
    where
    go (DartExpr.Var _ name) = foldMap S.singleton $ M.lookup name mnReverseLookup
    go _ = mempty
