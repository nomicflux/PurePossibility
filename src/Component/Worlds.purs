module Component.Worlds where

import Prelude

import Component.SVG as SVG
import Component.World as W
import Data.Array (filter, (:))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

data CanvasState = AddingWorlds | RemovingWorlds | DraggingWorlds | AddingRelations

type State =
  { worlds :: Array W.Id
  , maxId :: Int
  , canvasState :: CanvasState
  }

initialState :: State
initialState =
  { worlds: []
  , maxId: 0
  , canvasState: AddingWorlds
  }

data Query a =
  AddWorld MouseEvent a
  | ChangeCanvasState CanvasState a
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

renderCanvas :: forall m. Array W.Id ->
               HH.HTML (H.ComponentSlot HH.HTML W.Query m W.Slot (Query Unit)) (Query Unit)
renderCanvas worlds =
  SVG.svg [ SVG.width 500
          , SVG.height 500
          , SVG.viewBox "0 0 500 500"
          , SVG.class_ "world-svg"
          , HE.onClick (HE.input AddWorld)
          ] (renderWorld <$> worlds)


render :: forall m. State -> H.ParentHTML Query W.Query W.Slot m
render state =
  HH.div [] [ renderCanvas state.worlds ]

eval :: forall m. Query ~> H.ParentDSL State Query W.Query W.Slot Void m
eval = case _ of
  AddWorld mouseEvent next -> do
    let
      x = clientX mouseEvent
      y = clientY mouseEvent
    nextId <- H.gets _.maxId
    worlds <- H.gets _.worlds
    H.modify_ (_ { worlds = nextId : worlds, maxId = nextId + 1})
    _ <- H.query (W.Slot nextId) $ H.request (W.ChangePosition x y)
    pure next
  RemoveWorld id next -> do
    worlds <- H.gets _.worlds
    let newWorlds = filter (_ /= id) worlds
    H.modify_ (_ { worlds = newWorlds })
    pure next
  ChangeCanvasState newState next -> do
    H.modify_ (_ { canvasState = newState })
    pure next
  HandleWorld _query _id next -> do
    pure next
