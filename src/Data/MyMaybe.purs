module Data.MyMaybe where

import Prelude
import Class.MyFunctor (class MyFunctor)
import Data.Foldable (class Foldable, null)
import Helper (notImplemented)

data MyMaybe a
  = None
  | Some a

derive instance genericMyMaybe :: Generic (MyMaybe a) _

instance showMyMaybe :: Show a => Show (MyMaybe a) where
  show = genericShow

instance eqMyMaybe :: Eq a => Eq (MyMaybe a) where
  eq = notImplemented

instance myFunctorMaybe :: MyFunctor MyMaybe where
  fmap f = case _ of
    None -> None
    Some a -> Some $ f a

isSome :: forall a. MyMaybe a -> Boolean
isSome = not <<< null

instance foldableMyMaybe :: Foldable MyMaybe where
  foldr :: forall a b. (a -> b -> b) -> b -> MyMaybe a -> b
  foldr f accZero = case _ of
    None -> accZero
    Some a -> f a accZero
  foldl :: forall a b. (b -> a -> b) -> b -> MyMaybe a -> b
  foldl f accZero = case _ of
    None -> accZero
    Some a -> f accZero a
  foldMap :: forall a m. Monoid m => (a -> m) -> MyMaybe a -> m
  foldMap f = case _ of
    None -> mempty
    Some a -> f a
