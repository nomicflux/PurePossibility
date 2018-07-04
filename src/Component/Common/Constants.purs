module Component.Common.Constants where

import Data.Int (toNumber)
import Prelude ((*))

worldRadius :: Int
worldRadius = 25

selfRelationRadius :: Number
selfRelationRadius = (toNumber worldRadius) * 0.75

arrowHeadLength :: Number
arrowHeadLength = (toNumber worldRadius) * 0.5
