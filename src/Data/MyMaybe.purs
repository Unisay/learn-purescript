module Data.MyMaybe where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Node.Stream (onFinish)

data MyMaybe a
  = None
  | Some a

derive instance genericMyMaybe :: Generic (MyMaybe a) _

instance showMyMaybe :: Show a => Show (MyMaybe a) where
  show = genericShow

instance eqMyMaybe :: Eq a => Eq (MyMaybe a) where
  eq = genericEq

isSome :: forall a. MyMaybe a -> Boolean
isSome = mymaybe false (const true)

mymaybe :: forall b a. b -> (a -> b) -> MyMaybe a -> b
mymaybe n f = case _ of
  None -> n
  Some x -> f x

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

instance functorMyMaybe :: Functor MyMaybe where
  map :: forall a b. (a -> b) -> MyMaybe a -> MyMaybe b
  map f = mymaybe None (f >>> Some)

instance applyMyMaybe :: Apply MyMaybe where
  apply :: forall a b. MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  apply mbf mba = mymaybe None (\f -> mymaybe None (Some <<< f) mba) mbf

instance applicativeMyMaybe :: Applicative MyMaybe where
  pure = Some

instance bindMyMaybe :: Bind MyMaybe where
  bind :: forall a b. MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  bind ma k = mymaybe None k ma

type Beer
  = String

type Girls
  = Int

type Fun
  = Unit

findBeer :: MyMaybe Beer
findBeer = Some "bud"

findGirls :: Beer -> MyMaybe Girls
findGirls = case _ of
  "porter" -> Some 3
  _ -> None

makeParty :: Beer -> Girls -> Fun
makeParty _ _ = unit

makePartyWithBeerAndGirls :: MyMaybe Fun
makePartyWithBeerAndGirls = do
  beer <- findBeer
  girls <- findGirls beer
  pure $ makeParty beer girls
