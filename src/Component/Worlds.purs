module Component.Worlds where

import Prelude

import Component.SVG as SVG
import Component.World as W
import Data.Array (filter, (:))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State =
  { worlds :: Array W.Id
  , maxId :: Int
  }

initialState :: State
initialState =
  { worlds: [0, 1]
  , maxId: 1
  }

data Query a =
  AddWorld a
  | RemoveWorld W.Id a
  | HandleWorld W.Id W.Message a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState : const initialState
  , render
  , eval
  , receiver : const Nothing
  }

renderWorld :: forall m.
               W.Id ->
               HH.HTML (H.ComponentSlot HH.HTML W.Query m W.Slot (Query Unit)) (Query Unit)
renderWorld id =
  HH.slot (W.Slot id) W.component id (HE.input $ HandleWorld id)

render :: forall m. State -> H.ParentHTML Query W.Query W.Slot m
render state =
  SVG.svg [ SVG.width 500
          , SVG.height 500
          , SVG.viewBox "0 0 500 500"
          ] (renderWorld <$> state.worlds)

eval :: forall m. Query ~> H.ParentDSL State Query W.Query W.Slot Void m
eval = case _ of
  AddWorld next -> do
    nextId <- H.gets _.maxId
    worlds <- H.gets _.worlds
    H.modify_ (_ { worlds = nextId : worlds, maxId = nextId + 1})
    pure next
  RemoveWorld id next -> do
    worlds <- H.gets _.worlds
    let newWorlds = filter (_ /= id) worlds
    H.modify_ (_ { worlds = newWorlds })
    pure next
  HandleWorld _query _id next -> do
    pure next
