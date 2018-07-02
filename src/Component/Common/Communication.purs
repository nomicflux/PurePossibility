module Component.Common.Communication where

import Prelude

import Data.Foldable as F
import Data.Maybe (Maybe)
import Data.Traversable as T
import Halogen as H

passAlong :: forall s f g p o m a. Eq p =>
             (H.Request g a) ->
             H.ParentDSL s f g p o m Unit
passAlong query = do
  slots <- H.getSlots
  let action slot = H.query slot $ H.request query
  _ <- T.traverse_ action slots
  pure unit

getBack :: forall s f g p o m a b.
           Eq p =>
           (b -> Maybe a -> b) -> b ->
           (H.Request g a) ->
           H.ParentDSL s f g p o m b
getBack accumulator def query = do
  slots <- H.getSlots
  let action acc slot = do
        these <- H.query slot $ H.request query
        pure $ accumulator acc these
  F.foldM action def slots
