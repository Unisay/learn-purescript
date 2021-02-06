module Applicativity where

import Prelude
import Homework.Todo (todo)

-- | CA stands for "Compose Applicatives"
-- | Here applicative functors f and g are nested: g inside f.
newtype CA (f :: Type -> Type) (g :: Type -> Type) a
  = CA (f (g a))

instance caFunctor :: (Functor f, Functor g) => Functor (CA f g) where
  map :: forall a b. (a -> b) -> CA f g a -> CA f g b
  map _ _ = todo "Homework: implement"

instance caApply :: (Apply f, Apply g) => Apply (CA f g) where
  apply :: forall a b. CA f g (a -> b) -> CA f g a -> CA f g b
  apply _ _ = todo "Homework: implement"

instance caApplicative :: (Applicative f, Applicative g) => Applicative (CA f g) where
  pure :: forall a. a -> CA f g a
  pure _ = todo "Homework: implement"

-- | CAP stands for "Compose Applicatives as Product"
-- | Here applicative functors f and g are placed side-by-side: f, g.
-- Homework: Implement CAP instances - functor, apply, applicative.
data CAP (f :: Type -> Type) (g :: Type -> Type) a
  = CAP (f a) (g a)

instance capFunctor :: (Functor f, Functor g) => Functor (CAP f g) where
  map :: forall a b. (a -> b) -> CAP f g a -> CAP f g b
  map _ _ = todo "Homework: implement"

instance capApply :: (Apply f, Apply g) => Apply (CAP f g) where
  apply :: forall a b. CAP f g (a -> b) -> CAP f g a -> CAP f g b
  apply _ _ = todo "Homework: implement"

instance capApplicative :: (Applicative f, Applicative g) => Applicative (CAP f g) where
  pure :: forall a. a -> CAP f g a
  pure _ = todo "Homework: implement"

-- | CAS stands for "Compose Applicatives as Sum"
-- | Here applicative functors f and g are mutually exclusive: either f or g.
-- Homework: Implement CAS instances - functor, apply, applicative.
data CAS (f :: Type -> Type) (g :: Type -> Type) a
  = CASF (f a)
  | CASG (g a)

instance casFunctor :: (Functor f, Functor g) => Functor (CAS f g) where
  map :: forall a b. (a -> b) -> CAS f g a -> CAS f g b
  map _ _ = todo "Homework: implement"

instance casApply :: (Apply f, Apply g) => Apply (CAS f g) where
  apply :: forall a b. CAS f g (a -> b) -> CAS f g a -> CAS f g b
  apply _ _ = todo "Homework: implement"

instance casApplicative :: (Applicative f, Applicative g) => Applicative (CAS f g) where
  pure :: forall a. a -> CAS f g a
  pure _ = todo "Homework: implement"
