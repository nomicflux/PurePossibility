module Component.Worlds where

import Prelude

import Component.Common.Communication (passAlongTo)
import Component.Common.Offset (getCoordinates)
import Component.Common.SVG as SVG
import Component.World as W
import Data.Array ((:))
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
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
  , selectedWorlds :: Set W.Id
  }

initialState :: State
initialState =
  { worlds: []
  , maxId: 0
  , canvasState: AddingWorlds
  , selectedWorlds: S.empty
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

clickedWorlds :: MouseEvent -> H.ParentDSL State Query W.Query W.Slot Void Aff (List W.Id)
clickedWorlds mouseEvent = do
  coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
  worlds <- H.gets (_.worlds >>> L.fromFoldable)
  let clicked id = H.query (W.Slot id) $ H.request (W.Clicked coordinates)
  L.filterM (fromMaybeM false <<< clicked) worlds

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
      ids <- clickedWorlds mouseEvent
      let removed = S.fromFoldable ids
          newWorlds = A.fromFoldable $ L.filter (\id -> not S.member id removed) worlds
      H.modify_ (_ { worlds = newWorlds })
      pure next
    DraggingWorlds -> do
      coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
      pure next
    AddingRelations -> do
      selected <- H.gets (_.selectedWorlds)
      ids <- clickedWorlds mouseEvent
      case S.isEmpty selected of
        true -> H.modify_ (_ { selectedWorlds = selected })
        false -> do
          passAlongTo (W.Slot <$> L.fromFoldable ids) (W.AddRelation selected)
          H.modify_ (_ { selectedWorlds = S.empty :: Set W.Id })
      pure next
eval (ChangeCanvasState newState next) = do
    H.modify_ (_ { canvasState = newState })
    pure next
eval (HandleWorld _query _id next) = pure next
