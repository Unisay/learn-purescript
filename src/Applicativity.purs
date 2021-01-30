module Applicativity where

import Prelude
import Homework.Todo (todo)

-- | CA stands for "Compose Applicatives"
-- | Here applicative functors f and g are nested: g inside f.
newtype CA (f :: Type -> Type) (g :: Type -> Type) a
  = CA (f (g a))

instance caFunctor :: (Functor f, Functor g) => Functor (CA f g) where
  map :: forall a b. (a -> b) -> CA f g a -> CA f g b
  map _ _ = todo "Implement"

instance caApply :: (Apply f, Apply g) => Apply (CA f g) where
  apply :: forall a b. CA f g (a -> b) -> CA f g a -> CA f g b
  apply _ _ = todo "Implement"

instance caApplicative :: (Applicative f, Applicative g) => Applicative (CA f g) where
  pure :: forall a. a -> CA f g a
  pure _ = todo "Implement"

-- | CAP stands for "Compose Applicatives as Product"
-- | Here applicative functors f and g are placed side-by-side: f, g.
-- Homework: Implement CAP instances - functor, apply, applicative.
data CAP (f :: Type -> Type) (g :: Type -> Type) a
  = CAP (f a) (g a)

-- | CAS stands for "Compose Applicatives as Sum"
-- | Here applicative functors f and g are mutually exclusive: either f or g.
-- Homework: Implement CAS instances - functor, apply, applicative.
data CAS (f :: Type -> Type) (g :: Type -> Type) a
  = CASF (f a)
  | CASG (g a)


