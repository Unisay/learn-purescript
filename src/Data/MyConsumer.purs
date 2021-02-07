module Data.MyConsumer where

import Prelude
import Class.MyFunctor (class MyFunctor)

data MyConsumer a
  = MyConsumer

instance functorMyConsumer :: MyFunctor MyConsumer where
  fmap :: forall a b. (a -> b) -> MyConsumer a -> MyConsumer b
  fmap _ MyConsumer = MyConsumer

to :: (forall a. a -> Unit) -> Unit
to _ = unit

from :: Unit -> (forall a. a -> Unit)
from _ _ = unit
