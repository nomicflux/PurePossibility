module Component.Relation where

import Prelude

import Component.Common.Constants (arrowHeadLength, selfRelationRadius, worldRadius)
import Data.Array as A
import Data.Int (round, toNumber)
import Data.List (List(..))
import Data.List as L
import Halogen as H
import HalogenHelpers.Coordinates (Coordinates, Offset, addOffset, subOffset)
import HalogenHelpers.SVG as SVG
import Math as Math


getAngle :: Coordinates -> Coordinates -> Number
getAngle from to =
  let
    xdiff = to.x - from.x
    ydiff = to.y - from.y
  in
   Math.atan2 ydiff xdiff

getCircleOffset :: Number -> Number -> Offset
getCircleOffset angle multiplier =
  { left: multiplier * Math.cos angle
  , top: multiplier * Math.sin angle
  }

mkLine :: forall f g p m.
          Coordinates -> Coordinates -> String ->
          H.ParentHTML f g p m
mkLine from to class_ =
  SVG.line [ SVG.x1 $ round from.x
           , SVG.y1 $ round from.y
           , SVG.x2 $ round to.x
           , SVG.y2 $ round to.y
           , SVG.class_ class_
           ]

renderRelations :: forall f g p m.
                   String ->
                   List Coordinates ->
                   List Coordinates ->
                   Array (H.ParentHTML f g p m)
renderRelations class_ from to =
  A.fromFoldable $ do
    fromCoord <- from
    toCoord <- to
    case eqCoord fromCoord toCoord of
      true -> L.singleton (renderSelfRelation class_ fromCoord)
      false -> Cons (renderOtherRelation class_ fromCoord toCoord) (L.fromFoldable $ renderArrow class_ fromCoord toCoord)
  where
    eqCoord :: Coordinates -> Coordinates -> Boolean
    eqCoord f t = f.x == t.x && f.y == t.y

renderArrow :: forall f g p m.
               String ->
               Coordinates ->
               Coordinates ->
               Array (H.ParentHTML f g p m)
renderArrow class_ f t =
  let
    angle = getAngle f t
    mainOffset = getCircleOffset angle (toNumber worldRadius)
    onCircle = subOffset t mainOffset

    leftAngle = angle + 0.75 * Math.pi
    rightAngle = leftAngle + Math.pi / 2.0
    leftOffset = getCircleOffset leftAngle arrowHeadLength
    rightOffset = getCircleOffset rightAngle arrowHeadLength

    classes = "relation relation-arrow relation-" <> class_
  in
   [ mkLine onCircle (addOffset onCircle leftOffset) classes
   , mkLine onCircle (addOffset onCircle rightOffset) classes
   ]

renderSelfRelation :: forall f g p m.
                      String ->
                      Coordinates ->
                      H.ParentHTML f g p m
renderSelfRelation class_ c =
  let
    numRadius = toNumber worldRadius
    offsetY = numRadius * (Math.sqrt 3.0 / 2.0)
    offsetX = numRadius * (1.0 / 2.0)
  in
   SVG.arc [ SVG.circlePath (c.x + offsetX) (c.y + offsetY) selfRelationRadius (c.x - offsetX) (c.y + offsetY)
           , SVG.class_ $ "relation self-relation relation-" <> class_
           ]

renderOtherRelation :: forall f g p m.
                       String ->
                       Coordinates ->
                       Coordinates ->
                       H.ParentHTML f g p m
renderOtherRelation class_ f t =
  let
    angle = getAngle f t
    offset = getCircleOffset angle (toNumber worldRadius)
    onFromCircle = addOffset f offset
    onToCircle = subOffset t offset
  in
   mkLine onFromCircle onToCircle $ "relation other-relation relation-" <> class_
