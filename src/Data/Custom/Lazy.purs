module Data.Custom.Lazy where

import Prelude

-- | The `Lazy` class represents types which allow evaluation of values
-- | to be _deferred_.
-- |
-- | Usually, this means that a type contains a function arrow which can
-- | be used to delay evaluation.
class Lazy l where
  defer :: (Unit -> l) -> l

instance lazyFn :: Lazy (a -> b) where
  defer :: (Unit -> a -> b) -> a -> b
  defer f = \x -> f unit x

instance lazyUnit :: Lazy Unit where
  defer _ = unit

-- | `fix` defines a value as the fixed point of a function.
-- |
-- | The `Lazy` instance allows us to generate the result lazily.
fix :: ∀ l. Lazy l => (l -> l) -> l
fix f = go
  where
  go = defer \_ -> f go
