module Data.MyMaybe where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Helper (notImplemented)

data MyMaybe a
  = None
  | Some a

derive instance genericMyMaybe :: Generic (MyMaybe a) _

instance showMyMaybe :: Show a => Show (MyMaybe a) where
  show = genericShow

instance eqMyMaybe :: Eq a => Eq (MyMaybe a) where
  eq = notImplemented

isNone :: forall a. MyMaybe a -> Boolean
isNone (Some _) = true

isNone None = false

isSome :: forall a. MyMaybe a -> Boolean
isSome = not <<< isNone
