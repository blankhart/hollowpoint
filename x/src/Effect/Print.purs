module Effect.Print (print) where

import Prelude (Unit)
import Effect (Effect)

foreign import print :: String -> Effect Unit
