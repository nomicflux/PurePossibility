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

data Query a = ChangePosition Int Int a | ChangeColor String a

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
scale = 10

render :: State -> H.ComponentHTML Query
render state =
  let
    boxRadius = state.r + offset
    width = boxRadius
    height = width
  in
   SVG.svg [ SVG.width $ scale * width
           , SVG.height $ scale * height
           , SVG.viewBox (intercalate " " [ show (-width)
                                          , show (-height)
                                          , show $ width * 2
                                          , show $ height * 2
                                          ])
           ]
           [ SVG.circle [ SVG.cx state.x
                        , SVG.cy state.y
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
  ChangePosition x y next -> do
    H.modify_ (_ { x = x, y = y})
    pure next
  ChangeColor color next -> do
    H.modify_ (_ { color = color })
    pure next
