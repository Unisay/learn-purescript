module Data.MyReader where

import Data.Generic.Rep (class Generic)
import Helper (notImplemented)

newtype MyReader r a
  = MyReader (r -> a)

runMyReader :: forall r a. r -> MyReader r a -> a
runMyReader = notImplemented

derive instance genericMyReader :: Generic (MyReader r a) _
