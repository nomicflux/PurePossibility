module Component.Common.Coordinates where

import Prelude ((+), (-))

type Offset = { top :: Number
              , left :: Number
              }

noOffset :: Offset
noOffset = { top: 0.0
           , left: 0.0
           }

type Coordinates = { x :: Number
                   , y :: Number
                   }

subOffset :: Coordinates -> Offset -> Coordinates
subOffset coordinates offset =
  { x: coordinates.x - offset.left
  , y: coordinates.y - offset.top
  }

addOffset :: Coordinates -> Offset -> Coordinates
addOffset coordinates offset =
  { x: coordinates.x + offset.left
  , y: coordinates.y + offset.top
  }
