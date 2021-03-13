module Data.MyConst where

import Prelude
import Class.MyFunctor (class MyFunctor)
import Data.Generic.Rep

data MyConst c a
  = MyConst c

derive instance genericMyConst :: Generic (MyConst c a) _

instance showMyConst :: Show c => Show (MyConst c a) where
  show (MyConst c) = "MyConst (" <> show c <> ")"

instance myConst :: MyFunctor (MyConst c) where
  fmap :: forall a b. (a -> b) -> (MyConst c a -> MyConst c b)
  fmap _ (MyConst c) = MyConst c
