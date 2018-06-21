module Component.World where

import Data.Function (const)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen.Component as H
import Halogen.HTML as HH

type State =
  { x :: Number
  , y :: Number
  }

data Query a = ChangePosition Number Number a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
  { initialState : const initialState
  , render
  , eval
  , receiver : const Nothing
  }

initialState :: State
initialState = { x: 0, y: 0 }

render :: State -> H.ComponentHTML Query
render state = _
