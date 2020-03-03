{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Data.Version (makeVersion, showVersion)

#ifndef RELEASE
import qualified Development.GitRev as GitRev
#endif

versionString :: String
versionString = showVersion (makeVersion [0, 1, 0]) ++ extra
  where
#ifdef RELEASE
  extra = ""
#else
  extra = " [development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
  dirty
    | $(GitRev.gitDirty) = " DIRTY"
    | otherwise = ""
#endif
