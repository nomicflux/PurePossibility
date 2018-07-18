module Component.Worlds where

import Prelude

import Component.Relation (renderRelations)
import Component.World as W
import Data.Array as A
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
import HalogenHelpers.Communication (getBackFrom, passAlongTo)
import HalogenHelpers.Coordinates (Coordinates)
import HalogenHelpers.MouseOffset (getCoordinates)
import HalogenHelpers.SVG as SVG
import Logic.Properties as P
import Logic.RelationMap (RelationMap)
import Logic.RelationMap as R
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

data SystemAttribute = Reflexive | Symmetric | Transitive
derive instance eqSystemAttribute :: Eq SystemAttribute
derive instance ordSystemAttribute :: Ord SystemAttribute

data CanvasState = AddingWorlds
                 | AddingRelations
derive instance eqCanvasState :: Eq CanvasState

type State =
  { worlds :: Set W.Id
  , maxId :: Maybe Int
  , canvasState :: CanvasState
  , selectedWorlds :: Set W.Id
  , draggingWorlds :: Set W.Id
  , mouseAt :: List Coordinates
  , coordinateMap :: Map W.Id Coordinates
  , relationMap :: RelationMap
  , systemAttributes :: Set SystemAttribute
  }

initialState :: State
initialState =
  { worlds: S.empty
  , maxId: Nothing
  , canvasState: AddingWorlds
  , selectedWorlds: S.empty
  , draggingWorlds: S.empty
  , mouseAt: Nil
  , coordinateMap: M.empty
  , relationMap: R.empty
  , systemAttributes: S.empty
  }

data Query a =
  Click MouseEvent a
  | MouseMove MouseEvent a
  | ChangeAttribute SystemAttribute Boolean a
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

render :: forall m. State -> H.ParentHTML Query W.Query W.Slot m
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  , renderCanvas $ A.fromFoldable state.worlds
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
                , HP.type_ $ HP.ButtonButton
                ]
      [ HH.text text ]

    mkCheckbox :: String -> (Boolean -> Unit -> Query Unit) ->
                  H.ParentHTML Query W.Query W.Slot m
    mkCheckbox text query =
      HH.div_ [ HH.input [ HP.class_ (HH.ClassName "attr-checkbox")
                         , HP.type_ HP.InputCheckbox
                         , HE.onChecked $ HE.input query
                         , HP.title text
                         ]
              , HH.label_ [HH.text text]
              ]

    renderSidebar :: H.ParentHTML Query W.Query W.Slot m
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1-4 sidebar" ]
      [ mkButton "Add / Remove Worlds" "secondary" (state.canvasState == AddingWorlds) (ChangeCanvasState AddingWorlds)
      , mkButton "Add / Remove Relations" "success" (state.canvasState == AddingRelations) (ChangeCanvasState AddingRelations)
      , HH.form [ HP.class_ $ HH.ClassName "pure-form" ]
        [ mkCheckbox "Reflexive" (ChangeAttribute Reflexive)
        , mkCheckbox "Symmetric" (ChangeAttribute Symmetric)
        , mkCheckbox "Transitive" (ChangeAttribute Transitive)
        ]
      ]

    renderCanvas :: Array W.Id ->
                    H.ParentHTML Query W.Query W.Slot m
    renderCanvas worlds =
      let
        addingFromCoords =
          L.catMaybes $ (flip M.lookup state.coordinateMap) <$> L.fromFoldable state.selectedWorlds
        addingToCoords = state.mouseAt
        addingRelations = renderRelations "adding" addingFromCoords addingToCoords

        relWorlds = A.fromFoldable state.worlds
        existingRelations = A.concatMap (renderRelationsFrom state.coordinateMap "selected" state.relationMap) relWorlds

        genRels = applyAttributes state.systemAttributes state.relationMap
        justNew = R.newRelations state.relationMap genRels
        generatedRelations = A.concatMap (renderRelationsFrom state.coordinateMap "generated" justNew) relWorlds
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
                 ] $ (renderWorld <$> worlds) <> addingRelations <> existingRelations <> generatedRelations
       ]

applyAttributes :: Set SystemAttribute -> RelationMap -> RelationMap
applyAttributes attrs =
  applyTransitive <<< applySymmetric <<< applyReflexive
  where
    applyReflexive r = if S.member Reflexive attrs then P.reflexive r else r
    applySymmetric r = if S.member Symmetric attrs then P.symmetric r else r
    applyTransitive r = if S.member Transitive attrs then P.transitive r else r

renderRelationsFrom :: forall m.
                       Map W.Id Coordinates ->
                       String -> RelationMap ->
                       W.Id ->
                       Array (H.ParentHTML Query W.Query W.Slot m)
renderRelationsFrom coords class_ rels w =
  maybe [] (\(Tuple from to) -> renderRelations class_ (L.singleton from) to) (getRelCoordinates coords rels w)

getRelCoordinates :: Map W.Id Coordinates -> RelationMap -> W.Id -> Maybe (Tuple Coordinates (List Coordinates))
getRelCoordinates coords rels id = do
  fromCoords <- M.lookup id coords
  let
    fromRels = R.getRelationsFrom id rels
    toCoords = L.foldl (\acc i -> Cons (M.lookup i coords) acc) Nil (L.fromFoldable fromRels)
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

getNextId :: forall m. H.ParentDSL State Query W.Query W.Slot Void m W.Id
getNextId = do
  state <- H.get
  let allIds = maybe S.empty (S.fromFoldable <<< A.range 0) state.maxId
      missing = S.difference allIds state.worlds
  case S.isEmpty missing of
    false -> pure $ fromMaybe 0 (S.findMin missing)
    true -> do
      let newMax = maybe 0 (_ + 1) state.maxId
          newVars = R.addVariable state.relationMap
      H.modify_ (_ { maxId = Just newMax
                   , relationMap = newVars
                   })
      pure newMax

keyPressed :: MouseEvent -> Boolean
keyPressed mouseEvent =
  ME.altKey mouseEvent || ME.shiftKey mouseEvent || ME.metaKey mouseEvent

rightClicked :: MouseEvent -> Boolean
rightClicked mouseEvent =
  let x = ME.button mouseEvent
  in x == 2 || x == 3

eval :: Query ~> H.ParentDSL State Query W.Query W.Slot Void Aff
eval (Click mouseEvent next) =
  H.gets (_.canvasState) >>= case _ of
    AddingWorlds -> do
      worlds <- H.gets _.worlds
      case (keyPressed mouseEvent || rightClicked mouseEvent) of
        true -> do
          ids <- clickedWorlds mouseEvent
          coordinateMap <- H.gets (_.coordinateMap)
          relationMap <- H.gets (_.relationMap)
          let removed = S.fromFoldable ids
              newWorlds = S.difference worlds removed
              newCMap = L.foldl (flip M.delete) coordinateMap ids
              newRMap = L.foldl (flip R.rmVariable) relationMap ids
          H.modify_ (_ { worlds = newWorlds
                       , coordinateMap = newCMap
                       , relationMap = newRMap
                       })
          pure next
        false -> do
          coordinates <- H.getRef canvasRef >>= (H.liftEffect <<< getCoordinates mouseEvent)
          nextId <- getNextId
          coordinateMap <- H.gets _.coordinateMap
          let newMap = M.insert nextId coordinates coordinateMap
          H.modify_ (_ { worlds = S.insert nextId worlds })
          _ <- H.query (W.Slot nextId) $ H.request (W.ChangePosition coordinates)
          pure next
    AddingRelations -> do
      ids <- clickedWorlds mouseEvent
      case (keyPressed mouseEvent || rightClicked mouseEvent) of
        true -> do
          _ <- passAlongTo (W.Slot <$> ids) W.RemoveRelations
          pure next
        false -> do
          selected <- H.gets (_.selectedWorlds)
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
eval (ChangeAttribute attr val next) = do
  attrs <- H.gets (_.systemAttributes)
  case val of
    true -> H.modify_ (_ { systemAttributes = S.insert attr attrs })
    false -> H.modify_ (_ { systemAttributes = S.delete attr attrs })
  pure next
eval (HandleWorld id (W.PositionChanged coordinates) next) = do
  coordMap <- H.gets (_.coordinateMap)
  let newMap = M.insert id coordinates coordMap
  H.modify_ (_ { coordinateMap = newMap })
  pure next
eval (HandleWorld id (W.RelationsChanged relations) next) = do
  relMap <- H.gets (_.relationMap)
  let newMap = R.replaceRelations id relations relMap
  H.modify_ (_ { relationMap = newMap })
  pure next
