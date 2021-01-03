module Data.MyPhantom where

import Prelude
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)

data MyPhantom a
  = MyPhantom

derive instance genericMyPhantom :: Generic (MyPhantom a) _

instance foldableMyMaybe :: Foldable MyPhantom where
  foldr :: forall a b. (a -> b -> b) -> b -> MyPhantom a -> b
  foldr _ b _ = b
  foldl :: forall a b. (b -> a -> b) -> b -> MyPhantom a -> b
  foldl _ b _ = b
  foldMap :: forall a m. Monoid m => (a -> m) -> MyPhantom a -> m
  foldMap _ _ = mempty
