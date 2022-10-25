module Control.Coroutine.Suspension.Functor where

import Custom.Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Functor.Coproduct (Coproduct)

newtype Consume a b = Consume (a → b)

derive newtype instance Functor (Consume a)

data Produce ∷ Type → Type → Type
data Produce a b

instance Functor (Produce a) where
  map f y = produce (fst y) (f (snd y))

produce ∷ ∀ a b. a → b → Produce a b
produce = runFn2 mkProduce

foreign import mkProduce ∷ ∀ a b. Fn2 a b (Produce a b)
foreign import fst ∷ ∀ a b. Produce a b → a
foreign import snd ∷ ∀ a b. Produce a b → b

-- | A suspension functor that makes a coroutine which supplies a request
-- | and requires a response before it can proceed.
type RequestResponse request resp a = Produce request (Consume resp a)

-- | A suspension functor that makes a coroutine which can either demand
-- | or supply a value every time it suspends, but not both at the same time.
-- type DemandSupply demand supply = Coproduct (Function demand) (Tuple supply)
data DemandSupply demand supply k
  = Demand (Consume demand k)
  | Supply (Produce supply k)

derive instance Functor (DemandSupply d s)

type Split a = Coproduct
  (Consume (Maybe a))
  (Coproduct (Produce a) (Produce a))

type Join a = Coproduct
  (Coproduct (Consume (Maybe a)) (Consume (Maybe a)))
  (Produce a)

