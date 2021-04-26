module Composition where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype Compose (f :: Type -> Type) (k :: Type -> Type) (a :: Type)
  = Compose (f (k a))

derive instance genericCompose :: Generic (Compose f k a) _

instance showCompose :: Show (f (k a)) => Show (Compose f k a) where
  show = genericShow

instance functorCompose :: (Functor f, Functor k) => Functor (Compose f k) where
  map ab (Compose fka) = Compose (map (map ab) fka)

instance applyCompose :: (Apply f, Apply k) => Apply (Compose f k) where
  apply :: forall a b. Compose f k (a -> b) -> Compose f k a -> Compose f k b
  apply (Compose fkab) (Compose fka) = Compose fkb
    where
    fkb = ado
      kab <- fkab
      ka <- fka
      in ado
        ab <- kab
        a <- ka
        in ab a

instance applicativeCompose ::
  (Applicative f, Applicative k) =>
  Applicative (Compose f k) where
  pure :: forall a. a -> Compose f k a
  pure = Compose <<< pure <<< pure

instance bindCompose :: (Monad f) => Bind (Compose f Maybe) where
  bind :: forall a b. Compose f Maybe a -> (a -> Compose f Maybe b) -> Compose f Maybe b
  bind (Compose fma) acfmb =
    Compose $ fma
      >>= case _ of
          Nothing -> pure Nothing
          Just a -> unwrapCompose $ acfmb a

unwrapCompose :: forall f k a. Compose f k a -> f (k a)
unwrapCompose (Compose f) = f

runMaybeT' :: forall f a. MaybeT f a -> f (Maybe a)
runMaybeT' (MaybeT f) = f

newtype MaybeT f a
  = MaybeT (f (Maybe a))

instance maybeTFunctor :: (Functor f) => Functor (MaybeT f) where
  map f (MaybeT fma) = MaybeT (map (map f) fma)

instance applyMaybeT :: (Apply f) => Apply (MaybeT f) where
  apply (MaybeT fmab) (MaybeT fma) =
    MaybeT ado
      mab <- fmab
      ma <- fma
      in ado
        ab <- mab
        a <- ma
        in ab a

instance applicativeMaybeT :: (Applicative f) => Applicative (MaybeT f) where
  pure = MaybeT <<< pure <<< pure

instance bindMaybeT :: (Applicative f, Bind f) => Bind (MaybeT f) where
  bind (MaybeT fma) acfmb =
    MaybeT $ fma
      >>= case _ of
          Nothing -> pure Nothing
          Just a -> runMaybeT' $ acfmb a

instance monadMaybeT :: (Applicative f, Bind f) => Monad (MaybeT f)
