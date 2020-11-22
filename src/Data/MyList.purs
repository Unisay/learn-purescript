module Data.MyList where

import Prelude
import Data.Generic.Rep (class Generic)
import Helper (notImplemented)

data MyList a
  = EmptyList
  | MyCons a (MyList a)

derive instance genericMyList :: Generic (MyList a) _

instance showMyList :: Show (MyList a) where
  show = notImplemented
