module Component.World where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

import Component.SVG as SVG

type State =
  { x :: Int
  , y :: Int
  , r :: Int
  }

data Query a = ChangePosition Int Int a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
  { initialState : const initialState
  , render
  , eval
  , receiver : const Nothing
  }

initialState :: State
initialState = { x: 0, y: 0, r: 5 }

render :: State -> H.ComponentHTML Query
render state =
  SVG.circle [ SVG.cx state.x, SVG.cy state.y, SVG.r state.r, SVG.stroke "black" ]

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  ChangePosition x y next -> do
    state <- H.get
    let nextState = state { x = x, y = y }
    H.put nextState
    pure next
