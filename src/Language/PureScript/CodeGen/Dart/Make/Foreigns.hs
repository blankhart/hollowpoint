{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.Dart.Make.Foreigns where

{-
import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Control.Monad.Writer.Strict (runWriterT)
import           Data.Function (on)
import           Data.Foldable (for_)
import           Data.List (foldl', sortBy)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import           Language.PureScript.AST
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Docs.Convert as Docs
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Linter
import           Language.PureScript.ModuleDependencies
import           Language.PureScript.Renamer
import           Language.PureScript.Sugar
import           Language.PureScript.TypeChecker
import           Language.PureScript.Make as Make
import           Language.PureScript.Make.BuildPlan
import qualified Language.PureScript.Make.BuildPlan as BuildPlan
import qualified Language.PureScript.Make.Cache as Cache
import           Language.PureScript.Make.Monad as Monad
import qualified Language.PureScript.CoreFn as CF
-}

import           Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Language.PureScript.CoreFn as CF
import           Language.PureScript.Make.Actions (RebuildPolicy)
import           Language.PureScript.Names (ModuleName)
import           System.Directory (doesFileExist)
import           System.FilePath (replaceExtension)

import           Language.PureScript.CodeGen.Dart.Make.Monad as Dart

-- | Infer the module name for a module by looking for the same filename with
-- a .dart extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules =
    fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let dartFile = replaceExtension path "dart"
      exists <- liftIO $ doesFileExist dartFile
      if exists
        then return (Just dartFile)
        else return Nothing

-- | Check that the declarations in a given PureScript module match with those
-- in its corresponding foreign module.
-- TODO: checkForeignDecls for Dart
checkForeignDecls :: CF.Module ann -> FilePath -> Dart.Make ()
checkForeignDecls _ _ = return ()
