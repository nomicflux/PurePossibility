module Logic.Properties
       ( Property
       , reflexive
       , symmetric
       , transitive
       ) where

import Data.Array as A
import Data.Function (($))
import Data.List (List)
import Data.List as L
import Logic.RelationMap (RelationMap)
import Logic.RelationMap as R
import Prelude ((-))

type Property =
  RelationMap ->
  RelationMap

varRange :: RelationMap -> List Int
varRange rels = L.range 0 ((R.numVars rels) - 1)

reflexive :: Property
reflexive rels =
  L.foldl (\acc var -> R.addRelation var var acc) rels (varRange rels)

symmetric :: Property
symmetric rels =
  L.foldl (\acc var -> R.addRelationsTo (R.getRelationsFrom var rels) var acc) rels (varRange rels)

transitive :: Property
transitive rels =
  L.foldl (\acc var ->
            R.addRelations var (A.concatMap (\v ->
                                              R.getRelationsFrom v rels)
                                $ R.getRelationsFrom var rels)
            acc) rels (varRange rels)
