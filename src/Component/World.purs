module Component.World where

import Prelude

import Component.Common.Offset (Coordinates)
import Component.Common.SVG as SVG
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Halogen as H
import Halogen.HTML as HH

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
  | AddRelation (Set Id) (Unit -> a)
  | ChangeColor String (Unit -> a)
  | Clicked Coordinates (Boolean -> a)

data Slot = Slot Id
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data Message = NoOp

component :: forall m. H.Component HH.HTML Query Id Message m
component =
  H.component
  { initialState : initialState
  , render
  , eval
  , receiver : const Nothing
  }

initialState :: Id -> State
initialState id = { id, x: 0, y: 0, r: 10, color: "black", relations: S.empty }

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
                        , SVG.fill "white"
                        ]
           , SVG.text [ SVG.stroke "black"
                      , SVG.x (state.x - 3)
                      , SVG.y (state.y - state.r - 2)
                      , SVG.class_ "world-id"
                      ] [ HH.text (show state.id) ]
           ]

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  ChangePosition coordinates reply -> do
    H.modify_ (_ { x = round coordinates.x
                 , y = round coordinates.y
                 })
    pure (reply unit)
  AddRelation ids reply -> do
    currRels <- H.gets (_.relations)
    H.modify_ (_ { relations = S.union currRels ids})
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
