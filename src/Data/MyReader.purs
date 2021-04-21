module Data.MyReader where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe

newtype MyReader r a
  = MyReader (r -> a)

instance functorMyReader :: Functor (MyReader r) where
  map :: forall a b. (a -> b) -> MyReader r a -> MyReader r b
  map f (MyReader g) = MyReader (f <<< g)

instance applyMyReader :: Apply (MyReader r) where
  apply :: forall a b. MyReader r (a -> b) -> MyReader r a -> MyReader r b
  apply (MyReader rab) (MyReader ra) = MyReader $ apply rab ra

instance applicativeMyReader :: Applicative (MyReader r) where
  pure = MyReader <<< pure

instance bindMyReader :: Bind (MyReader r) where
  bind (MyReader ra) f = MyReader \r -> let MyReader rb = f (ra r) in rb r

runMyReader :: forall r a. r -> MyReader r a -> a
runMyReader r (MyReader f) = f r

ask :: forall r. MyReader r r
ask = MyReader identity

derive instance genericMyReader :: Generic (MyReader r a) _

type Age
  = Int

type Beer
  = String

type Girls
  = String

type Fun
  = String

buyBeer :: MyReader Age (Maybe Beer)
buyBeer = ask <#> \age -> if age >= 18 then Just "Porter" else Nothing

pickGirls :: MyReader Age (Maybe Girls)
pickGirls = ado
  age <- ask
  in if age >= 21 then Just "Real WOW" else Nothing

makeFun :: MyReader Age (Beer -> Girls -> Fun)
makeFun =
  ask
    <#> \age beer girls -> if age > 23 then "Total Fun!" else "No Fun :("

makeSuperParty :: MyReader Age (Maybe Fun)
makeSuperParty = do -- MyReader Age a
  maybeBeer <- buyBeer
  maybeGirls <- pickGirls
  f <- makeFun
  pure do -- Maybe a
    beer <- maybeBeer
    girls <- maybeGirls
    pure (f beer girls)

type MyReaderMaybe r a
  = MyReader r (Maybe a)
