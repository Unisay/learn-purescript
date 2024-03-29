module Data.Config where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)

newtype Config r a = Config (r -> a)

derive instance newtypeConfig :: Newtype (Config r a) _

instance functorConfig :: Functor (Config r) where
  map f (Config ra) = Config (f <<< ra)

runConfig :: forall r a. Config r a -> r -> a
runConfig = unwrap

config :: forall r. Config r r
config = Config identity

instance semigroupConfig :: Semigroup a => Semigroup (Config r a) where
  append (Config ral) (Config rar) = Config \r -> ral r <> rar r

instance applyConfig :: Apply (Config r) where
  apply :: forall r a b. Config r (a -> b) -> Config r a -> Config r b
  apply (Config rab) (Config ra) = Config \r -> rab r (ra r)

instance applicativeConfig :: Applicative (Config r) where
  pure :: forall a. a -> Config r a
  pure = wrap <<< const

instance bindConfig :: Bind (Config r) where
  bind :: forall r a b. Config r a -> (a -> Config r b) -> Config r b
  bind (Config ra) f = Config \r -> runConfig (f (ra r)) r

instance monadConfig :: Monad (Config r)

--------------------------------------------------------------------------------

data Verbosity = Loud | Quiet

instance showVerbosity :: Show Verbosity where
  show = case _ of
    Loud -> "Loud"
    Quiet -> "Quiet"

greeting :: Config Verbosity String
greeting = Config $ case _ of
  Loud -> "HELLO!"
  Quiet -> "hi"

currentVerbosity :: Config Verbosity Verbosity
currentVerbosity = Config identity

configurableProgram :: Config Verbosity String
configurableProgram = do
  gr <- greeting
  currentVerbosity <#> \cv ->
    gr <> "\n" <> case cv of
      Loud -> "I am very loud!"
      Quiet -> "I live my quiet life"

