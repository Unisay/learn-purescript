module Data.MyConsumer where

import Prelude
import Class.MyFunctor (class MyFunctor)
import Helper (notImplemented)

data MyConsumer a
  = MyConsumer (a -> Unit)

instance functorMyConsumer :: MyFunctor MyConsumer where
  fmap = notImplemented
