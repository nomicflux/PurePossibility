module Component.Common.MouseOffset where

import Component.Common.Coordinates (Coordinates, Offset, noOffset, subOffset)
import Control.Monad (pure)
import Data.Function (($))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (bind)
import Web.DOM.Element (Element)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

foreign import getScrollTop :: Effect Number

foreign import getScrollLeft :: Effect Number

foreign import getOffset :: Element -> Effect Offset

getMaybeOffset :: Maybe Element -> Effect Offset
getMaybeOffset (Just el) = getOffset el
getMaybeOffset Nothing = pure noOffset

getCoordinates :: MouseEvent -> Maybe Element -> Effect Coordinates
getCoordinates mouseEvent el = do
  let x = clientX mouseEvent
      y = clientY mouseEvent
      coordinates = { x: toNumber x
                    , y: toNumber y
                    }
  offset <- getMaybeOffset el
  pure $ subOffset coordinates offset
