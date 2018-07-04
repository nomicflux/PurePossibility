module Logic.RelationMap
       ( RelationMap
       , mkRelationMap
       , empty
       , numVars
       , addVariable
       , rmVariable
       , isRelation
       , addRelation
       , addRelations
       , addRelationsTo
       , rmRelation
       , rmRelations
       , flipRelation
       , replaceRelations
       , getRelationsFrom
       , newRelations
       ) where

import Control.Monad ((>>=))
import Data.Array ((!!))
import Data.Array as A
import Data.BooleanAlgebra (not)
import Data.Foldable (class Foldable)
import Data.Foldable as F
import Data.Function (const, flip, ($))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Prelude ((+), (==))

type RelationMap = Array (Array Boolean)

numVars :: RelationMap -> Int
numVars = A.length

mkRelationMap :: Int -> RelationMap
mkRelationMap n = A.replicate n (A.replicate n false)

empty :: RelationMap
empty = mkRelationMap 0

addVariable :: RelationMap -> RelationMap
addVariable relMap =
  let size = A.length relMap
      newRow = A.replicate (size + 1) false
      addedToRows = flip A.snoc false <$> relMap
  in
   A.snoc addedToRows newRow

isRelation :: Int -> Int -> RelationMap -> Boolean
isRelation a b rels =
  fromMaybe false $ rels !! a >>= (_ !! b)

updateRelation :: (Boolean -> Boolean) -> Int -> Int -> RelationMap -> RelationMap
updateRelation f a b rels =
  A.modifyAtIndices [a] (\r -> A.modifyAtIndices [b] f r) rels

addRelation :: Int -> Int -> RelationMap -> RelationMap
addRelation = updateRelation (const true)

addRelations :: forall f. Foldable f => Int -> f Int -> RelationMap -> RelationMap
addRelations fromId toIds rels =
  F.foldl (\acc toId -> addRelation fromId toId acc) rels toIds

addRelationsTo :: forall f. Foldable f => f Int -> Int -> RelationMap -> RelationMap
addRelationsTo fromIds toId rels =
  F.foldl (\acc fromId -> addRelation fromId toId acc) rels fromIds

rmRelation :: Int -> Int -> RelationMap -> RelationMap
rmRelation = updateRelation (const false)

rmRelations :: forall f. Foldable f => Int -> f Int -> RelationMap -> RelationMap
rmRelations fromId toIds rels =
  F.foldl (\acc toId -> rmRelation fromId toId acc) rels toIds

flipRelation :: Int -> Int -> RelationMap -> RelationMap
flipRelation = updateRelation not

setBlank :: Int -> RelationMap -> RelationMap
setBlank fromId rels =
  let size = A.length rels
      newRow = A.replicate (size + 1) false
  in
   A.updateAtIndices [Tuple fromId newRow] rels

replaceRelations :: forall f. Foldable f => Int -> f Int -> RelationMap -> RelationMap
replaceRelations fromId toIds rels =
  addRelations fromId toIds (setBlank fromId rels)

getRelationsFrom :: Int -> RelationMap -> Array Int
getRelationsFrom fromId rels =
  maybe [] (\row -> A.catMaybes $ A.mapWithIndex (\i a -> if a then Just i else Nothing) row) (rels !! fromId)

rmVariable :: Int -> RelationMap -> RelationMap
rmVariable id rels =
  (A.updateAtIndices [Tuple id false]) <$> (setBlank id rels)

newRelations :: RelationMap -> RelationMap -> RelationMap
newRelations fromRels toRels =
  let zippedOnce = A.zip fromRels toRels
      zippedTwice :: Array (Array (Tuple Boolean Boolean))
      zippedTwice = (\(Tuple a1 a2) -> A.zip a1 a2) <$> zippedOnce
  in
   (map (\(Tuple x y) -> if x == y then false else y)) <$> zippedTwice
