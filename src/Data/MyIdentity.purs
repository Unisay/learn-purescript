module Data.MyIdentity where

import Prelude
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)

newtype MyIdentity a
  = MyIdentity a

derive instance genericMyIdentity :: Generic (MyIdentity a) _

instance foldableMyMaybe :: Foldable MyIdentity where
  foldr :: forall a b. (a -> b -> b) -> b -> MyIdentity a -> b
  foldr f b (MyIdentity a) = f a b
  foldl :: forall a b. (b -> a -> b) -> b -> MyIdentity a -> b
  foldl f b (MyIdentity a) = f b a
  foldMap :: forall a m. Monoid m => (a -> m) -> MyIdentity a -> m
  foldMap f (MyIdentity a) = f a
