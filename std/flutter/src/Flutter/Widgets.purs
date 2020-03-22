module Flutter.Widgets where

import Effect (Effect)
import Prelude (Unit)
import Flutter.Foundation
import Flutter.Painting

-------------------------------------------------------
-- BuildContext

foreign import data BuildContext :: Type

-------------------------------------------------------
-- Widget

foreign import data Widget :: Type

foreign import runApp :: Widget -> Effect Unit

foreign import stateless :: forall props . (BuildContext -> props -> Widget) -> props -> Widget

foreign import text :: String -> Widget

foreign import center :: Widget -> Widget

-- foreign import stateful :: forall state props . props -> state -> Widget

-- foreign import align :: Maybe Key -> AlignmentGeometry -> Maybe Number -> Maybe Number -> Widget -> Widget


-------------------------------------------------------
-- BuildOwner

foreign import data BuildOwner :: Type

-------------------------------------------------------
-- Directionality

foreign import data Directionality :: Type

-------------------------------------------------------
-- Element

foreign import data Element :: Type

-------------------------------------------------------
-- Navigator

-------------------------------------------------------
-- Overlay

-------------------------------------------------------
-- Padding

foreign import data Padding :: Type

-------------------------------------------------------
-- Route

foreign import data Route :: Type

-------------------------------------------------------
-- RouteSettings

-------------------------------------------------------
-- Stack
