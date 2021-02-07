module Data.MyIdentity where

import Prelude
import Class.MyFunctor (class MyFunctor)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)

newtype MyIdentity a
  = MyIdentity a

derive instance genericMyIdentity :: Generic (MyIdentity a) _

derive newtype instance semiringMyIdentity ::
  Semiring a =>
  Semiring (MyIdentity a)

derive newtype instance ringMyIdentity ::
  Ring a =>
  Ring (MyIdentity a)

derive newtype instance semigroupMyIdentity ::
  Semigroup a =>
  Semigroup (MyIdentity a)

derive newtype instance monoidMyIdentity ::
  Monoid a =>
  Monoid (MyIdentity a)

instance foldableMyMaybe :: Foldable MyIdentity where
  foldr :: forall a b. (a -> b -> b) -> b -> MyIdentity a -> b
  foldr f b (MyIdentity a) = f a b
  foldl :: forall a b. (b -> a -> b) -> b -> MyIdentity a -> b
  foldl f b (MyIdentity a) = f b a
  foldMap :: forall a m. Monoid m => (a -> m) -> MyIdentity a -> m
  foldMap f (MyIdentity a) = f a

instance myFunctor :: MyFunctor MyIdentity where
  fmap :: forall a b. (a -> b) -> (MyIdentity a -> MyIdentity b)
  fmap f (MyIdentity a) = MyIdentity (f a)
