module Logic.Properties where

import Data.Foldable (class Foldable)
import Logic.RelationMap (RelationMap)

data Property f a = Property (
  Foldable f =>
  f a ->
  RelationMap ->
  RelationMap
  )
