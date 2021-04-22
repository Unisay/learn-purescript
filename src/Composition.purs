module Composition where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Homework.Todo (todo)

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

instance bindCompose :: (Monad f, Monad k) => Bind (Compose f k) where
  bind :: forall a b. Compose f k a -> (a -> Compose f k b) -> Compose f k b
  bind (Compose fka) acfkb = Compose $ fka >>= \ka -> pure (continue1 ka)
    where
    continue1 ka = ka <#> \a -> let Compose fkb = acfkb a in fkb
