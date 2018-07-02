module Component.Worlds where

import Prelude

import Component.Common.Offset (getCoordinates)
import Component.Common.SVG as SVG
import Component.World as W
import Data.Array ((:))
import Data.Array as A
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

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
  Click MouseEvent a
  | ChangeCanvasState CanvasState a
  | HandleWorld W.Id W.Message a

component :: H.Component HH.HTML Query Unit Void Aff
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

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "world-canvas"

renderCanvas :: forall m. Array W.Id ->
               HH.HTML (H.ComponentSlot HH.HTML W.Query m W.Slot (Query Unit)) (Query Unit)
renderCanvas worlds =
  HH.div [ HP.ref canvasRef
         , HP.class_ $ HH.ClassName "world-canvas"
         ]
  [ SVG.svg [ SVG.width 500
            , SVG.height 500
            , SVG.viewBox "0 0 500 500"
            , SVG.class_ "world-svg"
            , HE.onClick (HE.input Click)
            ] (renderWorld <$> worlds)
  ]

render :: forall m. State -> H.ParentHTML Query W.Query W.Slot m
render state =
  HH.div [] [ renderCanvas state.worlds ]

fromMaybeM :: forall a m.
              a ->
              H.ParentDSL State Query W.Query W.Slot Void m (Maybe a) ->
              H.ParentDSL State Query W.Query W.Slot Void m a
fromMaybeM def = map (fromMaybe def)

eval :: Query ~> H.ParentDSL State Query W.Query W.Slot Void Aff
eval (Click mouseEvent next) =
  H.gets (_.canvasState) >>= case _ of
    AddingWorlds -> do
      coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
      nextId <- H.gets _.maxId
      worlds <- H.gets _.worlds
      H.modify_ (_ { worlds = nextId : worlds, maxId = nextId + 1})
      _ <- H.query (W.Slot nextId) $ H.request (W.ChangePosition coordinates)
      pure next
    RemovingWorlds -> do
      worlds <- H.gets (_.worlds >>> L.fromFoldable)
      coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
      let clicked id = H.query (W.Slot id) $ H.request (W.Clicked coordinates)
      ids <- L.filterM (fromMaybeM false <<< clicked) worlds
      let removed = S.fromFoldable ids
          newWorlds = A.fromFoldable $ L.filter (\id -> not S.member id removed) worlds
      H.modify_ (_ { worlds = newWorlds })
      pure next
    DraggingWorlds -> do
      coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
      pure next
    AddingRelations -> do
      coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
      pure next
eval (ChangeCanvasState newState next) = do
    H.modify_ (_ { canvasState = newState })
    pure next
eval (HandleWorld _query _id next) = pure next
