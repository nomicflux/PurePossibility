module Component.World where

import Prelude

import Component.SVG as SVG
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

type Id = Int

type State =
  { id :: Id
  , x :: Int
  , y :: Int
  , r :: Int
  , color :: String
  }

data Query a = ChangePosition Int Int (Unit -> a) | ChangeColor String (Unit -> a)

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
initialState id = { id, x: 0, y: 0, r: 10, color: "black" }

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
   SVG.svg [ --SVG.width 500
           --, SVG.height 500
           ]
           [ SVG.circle [ SVG.cx $ state.x - state.r
                        , SVG.cy $ state.y - state.r
                        , SVG.r state.r
                        , SVG.stroke state.color
                        , SVG.fill "white"
                        ]
           , SVG.text [ SVG.stroke "black"
                      , SVG.x (state.x - state.r - 3)
                      , SVG.y (state.y - 2 * state.r - 2)
                      , SVG.class_ "world-id"
                      ] [ HH.text (show state.id) ]
           ]

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  ChangePosition x y reply -> do
    H.modify_ (_ { x = x, y = y})
    pure (reply unit)
  ChangeColor color reply -> do
    H.modify_ (_ { color = color })
    pure (reply unit)
