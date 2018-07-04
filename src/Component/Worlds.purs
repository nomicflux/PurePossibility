module Component.Worlds where

import Prelude

import Component.Common.Communication (getBackFrom, passAlongTo)
import Component.Common.Constants (arrowHeadLength, selfRelationRadius, worldRadius)
import Component.Common.Offset (Coordinates, getCoordinates)
import Component.Common.SVG as SVG
import Component.World as W
import Data.Array ((:))
import Data.Array as A
import Data.Int (round, toNumber)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (acos, asin, atan, atan2, cos, sin, sqrt)
import Math as Math
import Web.UIEvent.MouseEvent (MouseEvent)

data CanvasState = AddingWorlds
                 | RemovingWorlds
                 | DraggingWorlds
                 | AddingRelations
                 | RemovingRelations
derive instance eqCanvasState :: Eq CanvasState

type State =
  { worlds :: Array W.Id
  , maxId :: Int
  , canvasState :: CanvasState
  , selectedWorlds :: Set W.Id
  , mouseAt :: List Coordinates
  , coordinateMap :: Map W.Id Coordinates
  , relationMap :: Map W.Id (Set W.Id)
  }

initialState :: State
initialState =
  { worlds: []
  , maxId: 0
  , canvasState: AddingWorlds
  , selectedWorlds: S.empty
  , mouseAt: Nil
  , coordinateMap: M.empty
  , relationMap: M.empty
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
    case eqCoord fromCoord toCoord of
      true -> L.singleton (renderSelfRelation fromCoord)
      false -> Cons (renderRelation fromCoord toCoord) (renderArrow fromCoord toCoord)
  where
    eqCoord :: Coordinates -> Coordinates -> Boolean
    eqCoord f t = f.x == t.x && f.y == t.y

    renderArrow f t =
      let
        xdiff = t.x - f.x
        ydiff = t.y - f.y
        angle = Math.atan2 ydiff xdiff
        leftAngle = angle + 0.75 * Math.pi
        rightAngle = leftAngle + Math.pi / 2.0
        xOnCircle = t.x - toNumber worldRadius * (cos angle)
        yOnCircle = t.y - toNumber worldRadius * (sin angle)
        xOffsetLeft = arrowHeadLength * (cos leftAngle)
        xOffsetRight = arrowHeadLength * (cos rightAngle)
        yOffsetLeft = arrowHeadLength * (sin leftAngle)
        yOffsetRight = arrowHeadLength * (sin rightAngle)

      in
       L.fromFoldable [ SVG.line [ SVG.x1 $ round xOnCircle
                                 , SVG.y1 $ round yOnCircle
                                 , SVG.x2 $ round (xOnCircle + xOffsetLeft)
                                 , SVG.y2 $ round (yOnCircle + yOffsetLeft)
                                 , SVG.class_ "relation relation-arrow"
                                 ]
                      , SVG.line [ SVG.x1 $ round xOnCircle
                                 , SVG.y1 $ round yOnCircle
                                 , SVG.x2 $ round (xOnCircle + xOffsetRight)
                                 , SVG.y2 $ round (yOnCircle + yOffsetRight)
                                 , SVG.class_ "relation relation-arrow"
                                 ]
                      ]

    renderSelfRelation c =
      let
        numRadius = toNumber worldRadius
        offsetY = numRadius * (sqrt 3.0 / 2.0)
        offsetX = numRadius * (1.0 / 2.0)
      in
       SVG.arc [ SVG.circlePath (c.x + offsetX) (c.y + offsetY) selfRelationRadius (c.x - offsetX) (c.y + offsetY)
               , SVG.class_ "relation self-relation"
               ]

    renderRelation f t =
      let
        xdiff = t.x - f.x
        ydiff = t.y - f.y
        angle = atan2 ydiff xdiff
        xToCircle = toNumber worldRadius * (cos angle)
        yToCircle = toNumber worldRadius * (sin angle)
      in
       SVG.line [ SVG.x1 $ round (f.x + xToCircle)
                , SVG.y1 $ round (f.y + yToCircle)
                , SVG.x2 $ round (t.x - xToCircle)
                , SVG.y2 $ round (t.y - yToCircle)
                , SVG.class_ "relation other-relation"
                ]

render :: forall m. State -> H.ParentHTML Query W.Query W.Slot m
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  , renderCanvas state.worlds
  ]
  where
    mkButton :: String -> String ->
                Boolean ->
                (Unit -> Query Unit) ->
                H.ParentHTML Query W.Query W.Slot m
    mkButton text class_ disabled query =
      HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                , HE.onClick $ HE.input_ query
                , HP.disabled disabled
                ]
      [ HH.text text ]

    renderSidebar :: H.ParentHTML Query W.Query W.Slot m
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-4 sidebar" ]
      [ mkButton "Add Worlds" "secondary" (state.canvasState == AddingWorlds) (ChangeCanvasState AddingWorlds)
      , mkButton "Remove Worlds" "error" (state.canvasState == RemovingWorlds) (ChangeCanvasState RemovingWorlds)
      , mkButton "Add Relations" "success" (state.canvasState == AddingRelations) (ChangeCanvasState AddingRelations)
      , mkButton "Remove Relations" "primary" (state.canvasState == RemovingRelations) (ChangeCanvasState RemovingRelations)
      , mkButton "Drag Worlds" "warning" (state.canvasState == DraggingWorlds) (ChangeCanvasState DraggingWorlds)
      ]

    renderCanvas :: Array W.Id ->
                    H.ParentHTML Query W.Query W.Slot m
    renderCanvas worlds =
      let
        addingFromCoords =
          L.catMaybes $ (flip M.lookup state.coordinateMap) <$> L.fromFoldable state.selectedWorlds
        addingToCoords = state.mouseAt
        addingRelations = renderRelations addingFromCoords addingToCoords

        relWorlds = A.fromFoldable $ M.keys state.relationMap
        existingRelations = A.concatMap renderRelationsFrom relWorlds
      in
       HH.div [ HP.ref canvasRef
              , HP.class_ $ HH.ClassName "pure-u-3-4 world-canvas"
              ]
       [ SVG.svg [ SVG.width 1000
                 , SVG.height 1000
                 , SVG.viewBox "0 0 1000 1000"
                 , SVG.class_ "world-svg"
                 , HE.onClick (HE.input Click)
                 , HE.onMouseMove (HE.input MouseMove)
                 ] $ (renderWorld <$> worlds) <> addingRelations <> existingRelations
       ]

    renderRelationsFrom :: W.Id -> Array (H.ParentHTML Query W.Query W.Slot m)
    renderRelationsFrom w =
      maybe [] (\(Tuple from to) -> renderRelations (L.singleton from) to) (getRelCoordinates state w)

getRelCoordinates :: State -> W.Id -> Maybe (Tuple Coordinates (List Coordinates))
getRelCoordinates state id = do
  fromCoords <- M.lookup id state.coordinateMap
  fromRels <- M.lookup id state.relationMap
  let
    toCoords :: List (Maybe Coordinates)
    toCoords = L.foldl (\acc i -> Cons (M.lookup i state.coordinateMap) acc) Nil (L.fromFoldable fromRels)
  pure $ Tuple fromCoords (L.catMaybes toCoords)

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
                       H.ParentDSL State Query W.Query W.Slot Void Aff (Map W.Id Coordinates)
getWorldCoordinates worlds = do
  coordinateMap <- H.gets (_.coordinateMap)
  let ids = S.fromFoldable worlds
  pure $ M.filterKeys (\id -> S.member id ids) coordinateMap

getQueriedCoordinates :: List W.Id ->
                         H.ParentDSL State Query W.Query W.Slot Void Aff (Map W.Id (Maybe Coordinates))
getQueriedCoordinates worlds = do
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
      coordinateMap <- H.gets _.coordinateMap
      let newMap = M.insert nextId coordinates coordinateMap
      H.modify_ (_ { worlds = nextId : worlds
                   , maxId = nextId + 1
                   })
      _ <- H.query (W.Slot nextId) $ H.request (W.ChangePosition coordinates)
      pure next
    RemovingWorlds -> do
      worlds <- H.gets (_.worlds >>> L.fromFoldable)
      ids <- clickedWorlds mouseEvent
      coordinateMap <- H.gets (_.coordinateMap)
      relationMap <- H.gets (_.relationMap)
      let removed = S.fromFoldable ids
          newWorlds = A.fromFoldable $ L.filter (\id -> not S.member id removed) worlds
          newCMap = L.foldl (flip M.delete) coordinateMap ids
          newRMap = L.foldl (flip M.delete) relationMap ids
      H.modify_ (_ { worlds = newWorlds
                   , coordinateMap = newCMap
                   , relationMap = newRMap
                   })
      pure next
    DraggingWorlds -> do
      coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
      pure next
    AddingRelations -> do
      selected <- H.gets (_.selectedWorlds)
      ids <- clickedWorlds mouseEvent
      case S.isEmpty selected of
        true -> do
          newSelected <- clickedWorlds mouseEvent
          coords <- getWorldCoordinates newSelected
          H.modify_ (_ { selectedWorlds = S.fromFoldable newSelected
                       , mouseAt = M.values coords
                       })
        false -> do
          let
            relFrom = L.fromFoldable selected
            relTo = S.fromFoldable $ ids
          _ <- passAlongTo (W.Slot <$> relFrom) (W.AddRelation relTo)
          H.modify_ (_ { selectedWorlds = S.empty :: Set W.Id })
      pure next
    RemovingRelations -> do
      ids <- clickedWorlds mouseEvent
      _ <- passAlongTo (W.Slot <$> ids) W.RemoveRelations
      pure next
eval (MouseMove mouseEvent next) = do
  selected <- H.gets (_.selectedWorlds)
  baseCoordinates <-
      H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
  coordinates <- case S.isEmpty selected of
    true -> pure $ L.singleton baseCoordinates
    false -> do
      coords <- clickedWorlds mouseEvent >>= getWorldCoordinates
      pure $
        case M.values coords of
          Nil -> L.singleton baseCoordinates
          other -> other
  H.modify_ (_ { mouseAt = coordinates })
  pure next
eval (ChangeCanvasState newState next) = do
    H.modify_ (_ { canvasState = newState })
    pure next
eval (HandleWorld id (W.PositionChanged coordinates) next) = do
  coordMap <- H.gets (_.coordinateMap)
  let newMap = M.insert id coordinates coordMap
  H.modify_ (_ { coordinateMap = newMap })
  pure next
eval (HandleWorld id (W.RelationsChanged relations) next) = do
  relMap <- H.gets (_.relationMap)
  let newMap = M.insert id relations relMap
  H.modify_ (_ { relationMap = newMap })
  pure next
