module Component.Common.Offset where

import Control.Monad (pure)
import Data.Function (($))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude ((-), bind)
import Web.DOM.Element (Element)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

type Offset = { top :: Number
              , left :: Number
              }

noOffset :: Offset
noOffset = { top: 0.0
           , left: 0.0
           }

foreign import getScrollTop :: Effect Number

foreign import getScrollLeft :: Effect Number

foreign import getOffset :: Element -> Effect Offset

getMaybeOffset :: Maybe Element -> Effect Offset
getMaybeOffset (Just el) = getOffset el
getMaybeOffset Nothing = pure noOffset

type Coordinates = { x :: Number
                   , y :: Number
                   }

applyOffset :: Coordinates -> Offset -> Coordinates
applyOffset coordinates offset =
  { x: coordinates.x - offset.left
  , y: coordinates.y - offset.top
  }

getCoordinates :: MouseEvent -> Maybe Element -> Effect Coordinates
getCoordinates mouseEvent el = do
  let x = clientX mouseEvent
      y = clientY mouseEvent
      coordinates = { x: toNumber x
                    , y: toNumber y
                    }
  offset <- getMaybeOffset el
  pure $ applyOffset coordinates offset
