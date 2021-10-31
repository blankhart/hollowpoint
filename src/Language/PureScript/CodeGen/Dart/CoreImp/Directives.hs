module Language.PureScript.CodeGen.Dart.CoreImp.Directives where

import Protolude (ordNub)

import Data.List ((\\))
import qualified Data.Text as T

import qualified Language.PureScript.Constants.Prim as C
import Language.PureScript.Names

import Language.PureScript.CodeGen.Dart.Ident
import qualified Language.PureScript.CodeGen.Dart.CoreImp.AST as D
import Language.PureScript.CodeGen.Dart.CoreImp.AST (DartExpr)

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

