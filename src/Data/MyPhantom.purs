module Data.MyPhantom where

import Data.Generic.Rep (class Generic)

data MyPhantom a
  = MyPhantom

derive instance genericMyPhantom :: Generic (MyPhantom a) _
