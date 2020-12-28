module Data.Custom.Lazy where

import Prelude

newtype Lazy a
  = Lazy (Unit -> a)

force :: forall a. Lazy a -> a
force (Lazy thunk) = thunk unit

defer :: forall a. (Unit -> a) -> Lazy a
defer = Lazy

instance showLazy :: Show a => Show (Lazy a) where
  show _ = "<Unevaluated>"

instance functorLazy :: Functor Lazy where
  map f (Lazy thunk) = Lazy (map f thunk)

instance applyLazy :: Apply Lazy where
  apply f a = Lazy (\_ -> (force f) (force a))
