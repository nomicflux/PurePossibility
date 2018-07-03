module Component.Common.Communication where

import Prelude

import Data.Foldable (class Foldable)
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

passAlongTo :: forall l s f g p o m a. Eq p =>
               Foldable l =>
               l p ->
               (H.Request g a) ->
               H.ParentDSL s f g p o m Unit
passAlongTo slots query = do
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

getBackFrom :: forall l s f g p o m a b.
               Eq p =>
               Foldable l =>
               l p ->
               (b -> Maybe a -> b) -> b ->
               (H.Request g a) ->
               H.ParentDSL s f g p o m b
getBackFrom slots accumulator def query = do
  let action acc slot = do
        these <- H.query slot $ H.request query
        pure $ accumulator acc these
  F.foldM action def slots
