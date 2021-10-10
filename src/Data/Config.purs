module Data.Config where

import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype Config r a = Config (r -> a)

derive instance newtypeConfig :: Newtype (Config r a) _

instance functorConfig :: Functor (Config r) where
  map f (Config ra) = Config (f <<< ra)

runConfig :: forall r a. Config r a -> r -> a
runConfig = unwrap

instance semigroupConfig :: Semigroup a => Semigroup (Config r a) where
  append (Config ral) (Config rar) = Config \r -> ral r <> rar r

instance applyConfig :: Apply (Config r) where
  apply :: forall r a b. Config r (a -> b) -> Config r a -> Config r b
  apply (Config rab) (Config ra) = Config \r -> rab r (ra r)

instance bindConfig :: Bind (Config r) where
  bind :: forall r a b. Config r a -> (a -> Config r b) -> Config r b
  bind (Config ra) f = Config \r -> runConfig (f (ra r)) r
