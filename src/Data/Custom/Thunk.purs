module Data.Custom.Thunk where

import Prelude
import Data.Custom.Lazy (class Lazy)

newtype Thunk a
  = Thunk (Unit -> a)

force :: ∀ a. Thunk a -> a
force (Thunk thunk) = thunk unit

th :: ∀ a. (Unit -> a) -> Thunk a
th = Thunk

derive newtype instance semiringThunk :: Semiring a => Semiring (Thunk a)

derive newtype instance ringThunk :: Ring a => Ring (Thunk a)

derive newtype instance semigroupThunk :: Semigroup a => Semigroup (Thunk a)

derive newtype instance monoidThunk :: Monoid a => Monoid (Thunk a)

derive newtype instance applyThunk :: Apply Thunk

derive newtype instance functorThunk :: Functor Thunk

derive newtype instance applicativeThunk :: Applicative Thunk

derive newtype instance bindThunk :: Bind Thunk

instance showThunk :: Show a => Show (Thunk a) where
  show x = "(defer \\_ -> " <> show (force x) <> ")"

instance lazyThunk :: Lazy (Thunk a) where
  defer f = th \_ -> force (f unit)
