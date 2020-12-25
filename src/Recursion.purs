module Recursion where

import Prelude hiding (map)
import Debug.Trace (spy)

foo :: Int -> String
foo i = case i of
  1 -> "1"
  n -> foo (n - 1) <> "!"

foo' :: Int -> String
foo' i = go "" i
  where
  go :: String -> Int -> String
  go acc = case _ of
    1 -> "1" <> acc
    n -> go (acc <> "!") (n - 1)

call :: String -> String -> String
call _ _ = "call ok"

data List a
  = Empty
  | Cons a (List a)

instance showList :: Show a => Show (List a) where
  show = case _ of
    Empty -> "Empty"
    Cons h t -> show h <> " : " <> show t

infixr 6 Cons as :

visualize :: ∀ a. Show a => List a -> String
visualize a = case a of
  Empty -> "Empty"
  Cons h Empty -> "Cons " <> show h <> " Empty"
  Cons h t -> "Cons " <> show h <> " (" <> visualize t <> ")"

-- | Produce a `List` of size `Int` that seeds with the given value at its head,
-- | then runs the given function for subsequent elements
-- | ```purescript
-- | produce 5 not true = true : false : true : false : true : Empty
-- | produce 5 (_ * 2) 2 = 32 : 16 : 8 : 4 : 2 : Empty
-- | ```
produce :: ∀ a. Int -> (a -> a) -> a -> List a
produce 0 _ _ = Empty

produce size f head = go (head : Empty) size
  where
  go :: List a -> Int -> List a
  go acc remains = case remains, acc of
    1, _ -> acc
    _, Empty -> acc
    _, Cons h _ -> go (f h : acc) (remains - 1)

ints :: Int -> List Int
ints size = produce size (_ - 1) size

bools :: Int -> List Boolean
bools size = produce size not true

length' :: ∀ a. List a -> Int
length' = case _ of
  Empty -> 0
  Cons h t -> 1 + length t

length :: ∀ a. List a -> Int
length = go 0
  where
  go :: Int -> List a -> Int
  go acc = case _ of
    Empty -> acc
    Cons _ t -> go (add acc 1) t

-- | Returns the head of the list or the given default if list is empty.
-- | ```purescript
-- | headOr 1 Empty = 1
-- | headOr 9 (1 : 2 : Empty) = 1
-- | ````
headOr :: ∀ a. a -> List a -> a
headOr a = case _ of
  Empty -> a
  Cons h _ -> h

-- | Sum all elements of the list
-- | ```purescript
-- | sum (1 : 2 : 3 : Empty) = 6
-- | sum (1.0 : 2.0 : 3.0 : Empty) = 6.0
-- | ```
sum' :: ∀ x. Semiring x => List x -> x
sum' = case _ of
  Empty -> zero
  Cons h t -> h + sum t

sum :: ∀ x. Semiring x => List x -> x
sum xs = go zero xs
  where
  go :: x -> List x -> x
  go acc = case _ of
    Empty -> acc
    Cons h t -> go (acc + h) t

-- | Multiply all elements of the list
-- | ```purescript
-- | product (2 : 2 : Empty) = 4
-- | product (1.0 : 2.0 : 3.0 : Empty) = 6.0
-- | ```
product :: ∀ x. Semiring x => List x -> x
product = case _ of
  Empty -> zero
  xs -> go one xs
    where
    go :: x -> List x -> x
    go acc = case _ of
      Empty -> acc
      Cons h t -> go (acc * h) t

-- | Reverse elements of the list
-- | ```purescript
-- | reverse (1 : 2 : 3 : Empty) = (3 : 2 : 1 : Empty) 
-- | ```
-- | Law: 
-- | ```purescript
-- | ∀ xs. xs = reverse (reverse xs) 
-- | ```
reverse :: ∀ x. List x -> List x
reverse = go Empty
  where
  go :: List x -> List x -> List x
  go acc = case _ of
    Empty -> acc
    Cons h t -> go (h : acc) t

-- | Take first `n` elements of the list and return them as a result.
-- | ```purescript
-- | take 2 (1 : 2 : 3 : Empty) = 1 : 2 : Empty
-- | take 9 (1 : 2 : 3 : Empty) = 1 : 2 : 3 : Empty
-- | ```
take :: ∀ x. Int -> List x -> List x
take x = reverse <<< go Empty x
  where
  go :: List x -> Int -> List x -> List x
  go acc = case _, _ of
    0, _ -> acc
    _, Empty -> acc
    n, Cons h t -> go (h : acc) (n - 1) t

-- | Take first `n` elements of the list and return remaining elements
-- | as a result.
-- | ```purescript
-- | drop 2 (1 : 2 : 3 : Empty) = 3 : Empty
-- | drop 9 (1 : 2 : 3 : Empty) = Empty
-- | ```
drop :: ∀ x. Int -> List x -> List x
drop = go Empty
  where
  go :: List x -> Int -> List x -> List x
  go acc = case _, _ of
    _, Empty -> reverse acc
    n, Cons h t ->
      if n < 1 then
        go (h : acc) 0 t
      else
        go acc (n - 1) t

drop' :: ∀ x. Int -> List x -> List x
drop' = case _, _ of
  n, xs
    | n <= 0 -> xs
  _, Empty -> Empty
  n, Cons _ t -> drop (n - 1) t

-- | Apply given function to each element of the list
-- | producing a list of results.
-- | 
-- | ```purescript
-- | map' (_ + 10) (1 : 3 : 7 : Empty) = 11 : 13 : 17 : Empty
-- | map' (_ * 2) Empty = Empty
-- | map' identity (1 : 3 : 7 : Empty) = (1 : 3 : 7 : Empty)
-- | ```
map' :: ∀ a b. (a -> b) -> List a -> List b
map' f as = go identity as
  where
  go :: (List b -> List b) -> List a -> List b
  go acc = case _ of
    Empty -> acc Empty
    Cons h t -> go ((f h : _) >>> acc) t

-- | Filter elements of a list using provided predicate. 
-- | (Any function of type `a -> Boolean` is called a "predicate")
-- |
-- | Resulting list contains only elements for which applied predicate
-- | evaluated to `true`.
-- | 
-- | Examples:
-- | ```purescript
-- | filter (_ > 3) (1 : 2 : 4 : 8 : Empty) = 4 : 8 : Empty
-- | filter (_ > 3) Empty = Empty
-- | filter (_ == 100) (1 : 99 : Empty) = Empty
-- | filter identity (true : false : true : Empty) = true : true : Empty
-- | filter not (true : false : true : Empty) = false : Empty
-- | ```
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter p as = go identity as Empty
  where
  go :: (List a -> List a) -> List a -> (List a -> List a)
  go facc = case _ of
    Empty -> facc
    Cons h t -> go (if p h then (h : _) <<< facc else facc) t

fold :: forall a acc. (a -> acc -> Boolean) -> (a -> acc -> acc) -> acc -> List a -> acc
fold abort f acc = case _ of
  Empty -> acc
  Cons h t ->
    if abort h acc then
      acc
    else
      fold abort f (f h acc) t
