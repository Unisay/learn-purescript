module Applicative where

import Data.Unit
import Control.Apply (class Functor, map)
import Data.BooleanAlgebra (not)
-- import Data.Maybe (Maybe(..))
import Data.Semiring (add)
import Data.Tuple (Tuple)
import Homework.Todo (todo)

invert :: forall f. Functor f => f Boolean -> f Boolean
invert = map not

class
  Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

class
  Apply f <= Applicative f where
  pure :: forall a. a -> f a

-- instance applyMaybe :: Apply Maybe where
--   apply :: forall c d. Maybe (c -> d) -> Maybe c -> Maybe d
--   apply mcd mc = case mcd of
--     Nothing -> Nothing
--     Just cd -> case mc of
--       Nothing -> Nothing
--       Just c -> pure (cd c)
-- instance mbApplicative :: Applicative Maybe where
--   pure :: forall a. a -> Maybe a
--   pure = Just
apply2 :: forall a b c f. Apply f => f (a -> b -> c) -> f a -> f b -> f c
apply2 abcd fa fb = apply (apply abcd fa) fb

lift2 :: forall a b c f. Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 abcd fa fb = apply (map abcd fa) fb

apply3 :: forall a b c d f. Apply f => f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
apply3 abcd fa fb fc = apply (apply (apply abcd fa) fb) fc

lift3 :: forall a b c d f. Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 abcd fa fb fc = apply (apply (map abcd fa) fb) fc

apply4 :: forall a b c d e f. Apply f => f (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
apply4 abcd fa fb fc fe = apply (apply (apply (apply abcd fa) fb) fc) fe

lift4 :: forall a b c d e f. Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4 abcd fa fb fc fe = apply (apply (apply (map abcd fa) fb) fc) fe

{-
forall f.                                     ? ничего, просто отдать дальше или проигнорировать
  forall (f :: Type -> Type)                  ? + попробовать использовать его там где подходит кайнд
    forall (f :: Type -> Type). Functor =>    ? + делать apply1
      forall (f :: Type -> Type). Apply f =>  ? + делать apply2,3,4,....
-}
addF :: forall f. Apply f => f Int -> f Int -> f Int
addF f1 f2 = add `map` f1 `apply` f2

class
  Functor f <= Monoidal f where
  munit :: f Unit
  mapply :: forall a b. f a -> f b -> f (Tuple a b)

instance applicativeMonoidal :: Applicative f => Monoidal f where
  munit = todo "Implement using Applicative functions (map | pure | apply)"
  mapply _ _ = todo "Implement using Applicative functions (map | pure | apply)"

instance monoidalApply :: Monoidal f => Apply f where
  apply _ _ = todo "Implement using Monoidal functions (map | munit | mapply)"

instance monoidalApplicative :: Monoidal f => Applicative f where
  pure = todo "Implement using Monoidal functions (map | munit | mapply)"
