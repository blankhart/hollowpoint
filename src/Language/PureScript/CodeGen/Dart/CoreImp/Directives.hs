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

import Language.PureScript.CodeGen.Dart.Ident
import qualified Language.PureScript.CodeGen.Dart.CoreImp.AST as D
import Language.PureScript.CodeGen.Dart.CoreImp.AST (DartExpr)

import System.FilePath.Posix ((</>))

--  TODO: Implement more Dart-idiomatic renaming scheme that eliminates unnecessary characters and avoids a clash with module names.

getImportDirectives :: String -> FilePath -> ModuleName -> [ModuleName] -> [DartExpr]
getImportDirectives packageName libraryPrefix moduleName importNames =
  flip fmap requiredImports $ \mn ->
    D.Directive (D.Import (libraryUri mn) (fromModuleName mn))
  --  TODO: Filter out unused module imports?  If we construct a set of all used imports, do we even need to take the nub of the official list?  Since the Dart analyzer will do this it would just duplicate work, so perhaps continue suppressing the warning unless legibility is important.
  where
    requiredImports =
      ordNub importNames \\ (moduleName : C.primModules)
    libraryUri mn = T.pack $ "package:" <> toTargetImportName packageName libraryPrefix Index (T.unpack $ runModuleName mn)

getForeignDirectives :: String -> FilePath -> ModuleName -> [DartExpr]
getForeignDirectives packageName libraryPrefix moduleName =
  [ D.Directive (D.Import (libraryUri moduleName) "$foreign")
  , D.Directive (D.Export (libraryUri moduleName))
  ]
  where
    libraryUri mn = T.pack $ "package:" <> toTargetImportName packageName libraryPrefix Foreign (T.unpack $ runModuleName mn)

