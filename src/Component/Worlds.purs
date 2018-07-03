module Component.Worlds where

import Prelude

import Component.Common.Communication (getBackFrom, passAlongTo)
import Component.Common.Offset (Coordinates, getCoordinates)
import Component.Common.SVG as SVG
import Component.World as W
import Data.Array ((:))
import Data.Array as A
import Data.Int (round)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
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
  , selectedWorlds :: Map W.Id (Maybe Coordinates)
  , mouseAt :: List Coordinates
  }

initialState :: State
initialState =
  { worlds: []
  , maxId: 0
  , canvasState: AddingWorlds
  , selectedWorlds: M.empty
  , mouseAt: Nil
  }

data Query a =
  Click MouseEvent a
  | MouseMove MouseEvent a
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

renderRelations :: forall m.
                   List Coordinates ->
                   List Coordinates ->
                   Array (H.ParentHTML Query W.Query W.Slot m)
renderRelations from to =
  A.fromFoldable $ do
    fromCoord <- from
    toCoord <- to
    pure $ renderRelation fromCoord toCoord
  where
    renderRelation f t =
      SVG.line [ SVG.x1 $ round f.x
               , SVG.y1 $ round f.y
               , SVG.x2 $ round t.x
               , SVG.y2 $ round t.y
               , SVG.stroke "black"
               ]

render :: forall m. State -> H.ParentHTML Query W.Query W.Slot m
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  , renderCanvas state.worlds
  ]
  where
    mkButton :: String -> String ->
                (Unit -> Query Unit) ->
                H.ParentHTML Query W.Query W.Slot m
    mkButton text class_ query =
      HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                , HE.onClick $ HE.input_ query
                ]
      [ HH.text text ]

    renderSidebar :: H.ParentHTML Query W.Query W.Slot m
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-3 sidebar" ]
      [ mkButton "Add Worlds" "secondary" (ChangeCanvasState AddingWorlds)
      , mkButton "Remove Worlds" "error" (ChangeCanvasState RemovingWorlds)
      , mkButton "Add Relations" "success" (ChangeCanvasState AddingRelations)
      , mkButton "Drag Worlds" "warning" (ChangeCanvasState DraggingWorlds)
      ]

    renderCanvas :: Array W.Id ->
                    H.ParentHTML Query W.Query W.Slot m
    renderCanvas worlds =
      let
        fromCoords = L.catMaybes $ M.values state.selectedWorlds
        toCoords = state.mouseAt
      in
       HH.div [ HP.ref canvasRef
              , HP.class_ $ HH.ClassName "pure-u-2-3 world-canvas"
              ]
       [ SVG.svg [ SVG.width 500
                 , SVG.height 500
                 , SVG.viewBox "0 0 500 500"
                 , SVG.class_ "world-svg"
                 , HE.onClick (HE.input Click)
                 , HE.onMouseMove (HE.input MouseMove)
                 ] $ (renderWorld <$> worlds) <> (renderRelations fromCoords toCoords)
       ]



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

getWorldCoordinates :: List W.Id ->
                       H.ParentDSL State Query W.Query W.Slot Void Aff (Map W.Id (Maybe Coordinates))
getWorldCoordinates worlds = do
  coords <- getBackFrom (W.Slot <$> worlds) go Nil W.GetCoordinates
  let zipped = L.zip worlds (L.reverse coords)
  pure $ M.fromFoldable zipped
  where
    go :: List (Maybe Coordinates) -> Maybe Coordinates -> List (Maybe Coordinates)
    go = flip L.Cons

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
      case M.isEmpty selected of
        true -> do
          newSelected <- clickedWorlds mouseEvent >>= getWorldCoordinates
          H.modify_ (_ { selectedWorlds = newSelected })
        false -> do
          let
            relFrom = L.fromFoldable $ M.keys selected
            relTo = S.fromFoldable $ ids
          _ <- passAlongTo (W.Slot <$> relFrom) (W.AddRelation relTo)
          H.modify_ (_ { selectedWorlds = M.empty :: Map W.Id (Maybe Coordinates) })
      pure next
eval (MouseMove mouseEvent next) = do
  selected <- H.gets (_.selectedWorlds)
  baseCoordinates <-
      H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
  coordinates <- case M.isEmpty selected of
    true -> pure $ L.singleton baseCoordinates
    false -> do
      coords <- clickedWorlds mouseEvent >>= getWorldCoordinates
      pure $
        case L.catMaybes $ M.values coords of
          Nil -> L.singleton baseCoordinates
          other -> other
  H.modify_ (_ { mouseAt = coordinates })
  pure next
eval (ChangeCanvasState newState next) = do
    H.modify_ (_ { canvasState = newState })
    pure next
eval (HandleWorld _query _id next) = pure next
