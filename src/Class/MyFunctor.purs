module Class.MyFunctor where

import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))

class MyFunctor (f :: Type -> Type) where
  fmap :: forall a b. (a -> b) -> (f a -> f b)

instance myFunctor :: MyFunctor Maybe where
  fmap :: forall a b. (a -> b) -> (Maybe a -> Maybe b)
  fmap f = case _ of
    Just a -> Just (f a)
    Nothing -> Nothing

--instance myList :: MyFunctor List where
--  fmap :: forall a b. (a -> b) -> (List a -> List b)
-- fmap f = foldr (\a acc -> Cons (f a) acc) Empty
