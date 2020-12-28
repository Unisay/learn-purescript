module Data.Custom.Stream where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable
  ( class Unfoldable
  , class Unfoldable1
  , replicate
  , unfoldr
  , unfoldr1
  )
import Data.Custom.Lazy (Lazy, defer, force)

newtype Stream a
  = Stream (Lazy (Step a))

data Step a
  = Nil
  | Next a (Stream a)

-- | Unwrap a stream
step :: forall a. Stream a -> Step a
step = force <<< unwrap

-- | Empty stream
empty :: forall a. Stream a
empty = Stream $ defer \_ -> Nil

-- | Attach an element to the front of a Stream
cons :: forall a. a -> Stream a -> Stream a
cons a as = Stream $ defer \_ -> Next a as

-- | Single element stream
singleton :: forall a. a -> Stream a
singleton a = cons a empty

derive instance newtypeStream :: Newtype (Stream a) _

instance showCustomList :: Show a => Show (Stream a) where
  show xs =
    let
      Tuple ident s = foldl f (Tuple 0 "") xs
    in
      s <> pad ident <> "Nil"
    where
    f :: Tuple Int String -> a -> Tuple Int String
    f (Tuple ident acc) a =
      Tuple (ident + 1)
        (acc <> pad ident <> "Next " <> show a <> " \\_ ->\n")

    pad :: Int -> String
    pad x = fromCharArray (replicate (x * 2) ' ')

instance unfoldable1LazyList :: Unfoldable1 Stream where
  unfoldr1 gen seed = case gen seed of
    Tuple a Nothing -> cons a empty
    Tuple a (Just seed') -> Stream $ defer \_ -> Next a (unfoldr1 gen seed')

instance unfoldableLazyList :: Unfoldable Stream where
  unfoldr gen seed = case gen seed of
    Nothing -> empty
    Just (Tuple a seed') -> Stream $ defer \_ -> Next a (unfoldr gen seed')

-- https://www.joachim-breitner.de/various/foldl-foldr-original.png
instance foldableLazyList :: Foldable Stream where
  foldr :: forall a b. (a -> b -> b) -> b -> Stream a -> b
  foldr op z xs = case step xs of
    Nil -> z
    Next h t -> foldr op (op h z) t
  foldl :: forall a b. (b -> a -> b) -> b -> Stream a -> b
  foldl op z xs = case step xs of
    Nil -> z
    Next h t -> foldl op (op z h) t
  foldMap f xs = case step xs of
    Nil -> mempty
    Next h t -> f h <> foldMap f t

take :: forall a. Int -> Stream a -> Stream a
take n =
  if n <= 0 then
    const empty
  else
    Stream <<< map (go n) <<< unwrap
  where
  go :: Int -> Step a -> Step a
  go = case _, _ of
    _, Nil -> Nil
    i, Next h t -> Next h (take (i - 1) t)

filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter p = Stream <<< map go <<< unwrap
  where
  go :: Step a -> Step a
  go = case _ of
    Nil -> Nil
    Next h t
      | p h -> Next h (Stream $ map go $ unwrap t)
    Next _ t -> go (step t)

ints :: Int -> Stream Int
ints n = take n infinite

infinite :: forall n. Semiring n => Stream n
infinite = unfoldr (\b -> Just $ let b' = b + one in Tuple b' b') zero

repeat :: forall a. a -> Stream a
repeat a = Stream $ defer \_ -> Next a (repeat a)

zip :: forall a b. Stream a -> Stream b -> Stream (Tuple a b)
zip = zipWith (\a b -> Tuple a b)

zipWith :: forall a b c. (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith op as bs = Stream (go <$> unwrap as <*> unwrap bs)
  where
  go = case _, _ of
    _, Nil -> Nil
    Nil, _ -> Nil
    Next x tx, Next y ty -> Next (op x y) (zipWith op tx ty)
