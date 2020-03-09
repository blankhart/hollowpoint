{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CodeGen.Dart.Version where

import Data.Version (showVersion)
import qualified Paths_hollowpoint as Paths
-- import qualified Paths_purescript as PsPaths -- not exposed

#ifndef RELEASE
import qualified Development.GitRev as GitRev
#endif

versionString :: String
versionString = showVersion Paths.version ++ extra
  where
#ifdef RELEASE
  extra = ""
#else
  extra = " [development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
  dirty
    | $(GitRev.gitDirty) = " DIRTY"
    | otherwise = ""
#endif
