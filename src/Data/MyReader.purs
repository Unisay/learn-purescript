module Data.MyReader where

import Prelude
import Class.MyFunctor (class MyFunctor)
import Helper (notImplemented)

newtype MyReader r a
  = MyReader (r -> a)

instance myFunctor :: MyFunctor (MyReader r) where
  fmap :: forall a b. (a -> b) -> MyReader r a -> MyReader r b
  fmap f (MyReader g) = MyReader (f <<< g)

runMyReader :: forall r a. r -> MyReader r a -> a
runMyReader = notImplemented

derive instance genericMyReader :: Generic (MyReader r a) _
