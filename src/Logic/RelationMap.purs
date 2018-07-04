module Logic.RelationMap
       ( RelationMap
       , mkRelationMap
       , addVariable
       , isRelation
       , addRelation
       , rmRelation
       , flipRelation
       )where

import Control.Monad ((>>=))
import Data.Array ((!!))
import Data.Array as A
import Data.BooleanAlgebra (not)
import Data.Function (const, flip, ($))
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Prelude ((+))

type RelationMap = Array (Array Boolean)

mkRelationMap :: Int -> RelationMap
mkRelationMap n = A.replicate n (A.replicate n false)

addVariable :: RelationMap -> RelationMap
addVariable relMap =
  let size = fromMaybe 0 (A.length <$> relMap !! 0)
      newRow = A.replicate (size + 1) false
      addedToRows = flip A.snoc false <$> relMap
  in
   A.snoc addedToRows newRow

isRelation :: RelationMap -> Int -> Int -> Boolean
isRelation rels a b =
  fromMaybe false $ rels !! a >>= (_ !! b)

updateRelation :: (Boolean -> Boolean) -> RelationMap -> Int -> Int -> RelationMap
updateRelation f rels a b =
  A.modifyAtIndices [a] (\r -> A.modifyAtIndices [b] f r) rels

addRelation :: RelationMap -> Int -> Int -> RelationMap
addRelation = updateRelation (const true)

rmRelation :: RelationMap -> Int -> Int -> RelationMap
rmRelation = updateRelation (const false)

flipRelation :: RelationMap -> Int -> Int -> RelationMap
flipRelation = updateRelation not
