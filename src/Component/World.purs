module Component.World where

import Prelude

import Component.Common.Constants (worldRadius)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Halogen as H
import Halogen.HTML as HH
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.SVG as SVG

type Id = Int

type State =
  { id :: Id
  , x :: Int
  , y :: Int
  , r :: Int
  , color :: String
  , relations :: Set Id
  }

data Query a =
  ChangePosition Coordinates (Unit -> a)
  | GetCoordinates (Coordinates -> a)
  | AddRelation (Set Id) (Unit -> a)
  | RemoveRelations (Unit -> a)
  | ChangeColor String (Unit -> a)
  | Clicked Coordinates (Boolean -> a)

data Slot = Slot Id
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data Message = PositionChanged Coordinates | RelationsChanged (Set Id)

component :: forall m. H.Component HH.HTML Query Id Message m
component =
  H.component
  { initialState : initialState
  , render
  , eval
  , receiver : const Nothing
  }

initialState :: Id -> State
initialState id = { id, x: 0, y: 0, r: worldRadius, color: "black", relations: S.empty }

offset :: Int
offset = 5

scale :: Int
scale = 3

render :: State -> H.ComponentHTML Query
render state =
  let
    boxRadius = state.r + offset
    width = boxRadius
    height = width
  in
   SVG.svg [ ]
           [ SVG.circle [ SVG.cx $ state.x
                        , SVG.cy $ state.y
                        , SVG.r state.r
                        , SVG.stroke state.color
                        , SVG.class_ "world world-base"
                        ]
           , SVG.text [ SVG.stroke "black"
                      , SVG.x (state.x - 3)
                      , SVG.y (state.y - state.r - 2)
                      , SVG.class_ "world-id"
                      ] [ HH.text (show state.id) ]
           ]

getCoordinates :: State -> Coordinates
getCoordinates state =
  { x: toNumber state.x
  , y: toNumber state.y
  }

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  ChangePosition coordinates reply -> do
    H.modify_ (_ { x = round coordinates.x
                 , y = round coordinates.y
                 })
    H.raise $ PositionChanged coordinates
    pure (reply unit)
  GetCoordinates reply ->
    H.gets getCoordinates >>= (pure <<< reply)
  AddRelation ids reply -> do
    currRels <- H.gets (_.relations)
    let newRels = S.union currRels ids
    H.modify_ (_ { relations = newRels })
    H.raise $ RelationsChanged newRels
    pure $ reply unit
  RemoveRelations reply -> do
    let emptyRels = S.empty :: Set Id
    H.modify_ (_ { relations = emptyRels })
    H.raise $ RelationsChanged emptyRels
    pure $ reply unit
  ChangeColor color reply -> do
    H.modify_ (_ { color = color })
    pure (reply unit)
  Clicked coordinates reply -> do
    state <- H.get
    let minX = toNumber $ state.x - state.r
        maxX = toNumber $ state.x + state.r
        minY = toNumber $ state.y - state.r
        maxY = toNumber $ state.y + state.r
    pure $ reply (coordinates.x <= maxX && coordinates.x >= minX && coordinates.y <= maxY && coordinates.y >= minY)
