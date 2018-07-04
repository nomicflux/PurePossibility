module Logic.RelationMapST
       ( RelationMapST
       , withRelationMap
       , addRelationST
       , rmRelationST
       , flipRelationST
       )
       where

import Prelude

import Control.Monad.ST (ST)
import Data.Array ((!!))
import Data.Array as A
import Data.Array.ST (STArray)
import Data.Array.ST as AST
import Data.Array.ST.Iterator as ASTI
import Data.Traversable as T
import Logic.RelationMap (RelationMap)

data RelationMapST region = RMST (STArray region (STArray region Boolean))

thawRM :: forall r.
          RelationMap -> ST r (RelationMapST r)
thawRM rels =
  T.for rels AST.thaw >>= AST.thaw >>= (pure <<< RMST)

unsafeFreezeRM :: forall r.
                  (RelationMapST r) -> ST r RelationMap
unsafeFreezeRM (RMST strels) = do
  rows <- AST.unsafeFreeze strels
  T.for rows AST.unsafeFreeze

withRelationMap :: forall r b.
  (RelationMapST r -> ST r b) ->
  RelationMap ->
  ST r RelationMap
withRelationMap f rels = do
  strels <- thawRM rels
  _ <- f strels
  unsafeFreezeRM strels

iterate :: forall r.
           RelationMapST r ->
           (STArray r Boolean -> ST r Unit) ->
           ST r Unit
iterate (RMST strels) f = do
  rows <- AST.unsafeFreeze strels
  iterator <- (ASTI.iterator (rows !! _))
  _ <- ASTI.iterate iterator f
  pure unit

sizeST :: forall r.
          RelationMapST r ->
          ST r Int
sizeST (RMST r) = A.length <$> AST.unsafeFreeze r

addVariableST :: forall r.
                 (RelationMapST r) ->
                 ST r Unit
addVariableST strels@(RMST rows) = do
  size <- sizeST strels
  let newRow = A.replicate (size + 1) false
  newRowST <- AST.unsafeThaw newRow
  _ <- iterate strels (\row -> AST.push false row *> pure unit)
  _ <- AST.push newRowST rows
  pure unit

updateRelationST :: forall r.
                    (Boolean -> Boolean) -> RelationMapST r -> Int -> Int ->
                    ST r Unit
updateRelationST f strels a b =
  iterate strels (\row -> AST.modify b f row *> pure unit)

addRelationST :: forall r.
                 RelationMapST r -> Int -> Int ->
                 ST r Unit
addRelationST = updateRelationST (const true)

rmRelationST :: forall r.
                RelationMapST r -> Int -> Int ->
                ST r Unit
rmRelationST = updateRelationST (const false)

flipRelationST :: forall r.
                  RelationMapST r -> Int -> Int ->
                  ST r Unit
flipRelationST = updateRelationST not
