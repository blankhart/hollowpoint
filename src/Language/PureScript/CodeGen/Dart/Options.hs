-- | Options for code generation to Dart
module Language.PureScript.CodeGen.Dart.Options where

import Prelude.Compat
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as Map

-- | The data type of Dart-related code generation targets
data CodegenTarget
  = Dart
  | CoreFn
  | Docs
  deriving (Eq, Ord, Show)

-- | The data type of compiler options
data Options = Options
  { optionsVerboseErrors :: Bool
  -- ^ Verbose error message
  , optionsNoComments :: Bool
  -- ^ Remove the comments from the generated Dart
  , optionsCodegenTargets :: S.Set CodegenTarget
  -- ^ Codegen targets (dart, corefn, docs)
  } deriving Show

-- Default make options
defaultOptions :: Options
defaultOptions = Options False False (S.singleton Dart)

codegenTargets :: Map String CodegenTarget
codegenTargets = Map.fromList
  [ ("dart", Dart)
  , ("corefn", CoreFn)
  , ("docs", Docs)
  ]
