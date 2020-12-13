module Recursion where

import Prelude hiding (map)
import Homework.Todo (todo)

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
produce size f head = go (head : Empty) size
  where
  go acc remains = case remains, acc of
    1, _ -> acc
    _, Empty -> acc
    _, Cons h _ -> go (f h : acc) (remains - 1)

ints :: Int -> List Int
ints size = produce size (_ - 1) size

bools :: Int -> List Boolean
bools size = produce size not true

length :: ∀ a. List a -> Int
length = case _ of
  Empty -> 0
  Cons h t -> 1 + length t

length' :: ∀ a. List a -> Int
length' = go 0
  where
  go :: Int -> List a -> Int
  go acc = case _ of
    Empty -> acc
    Cons _ t -> go (acc + 1) t

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
sum ::
  ∀ x.
  Semiring x => -- | Semiring gives you `zero`, `one` and can `add` and `mul`
  List x -> -- List of `add`-able things
  x -- result of summing up (via `add`) all the elements in that list.
sum = go zero
  where
  go acc = case _ of
    Empty -> acc
    Cons h t -> go (add acc h) t

-- | Multiply all elements of the list
-- | ```purescript
-- | product (2 : 2 : Empty) = 4
-- | product (1.0 : 2.0 : 3.0 : Empty) = 6.0
-- | ```
product :: ∀ x. Semiring x => List x -> x
product = go one
  where
  go acc = case _ of
    Empty -> acc
    Cons h t -> go (mul acc h) t

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
  go acc = case _ of
    Empty -> acc
    Cons h t -> go (h : acc) t

-- | Take first `n` elements of the list and return them as a result.
-- | ```purescript
-- | take 2 (1 : 2 : 3 : Empty) = 1 : 2 : Empty
-- | take 9 (1 : 2 : 3 : Empty) = 1 : 2 : 3 : Empty
-- | ```
take :: ∀ x. Int -> List x -> List x
take num xx =
  if num < 0 then
    Empty
  else
    reverse (go Empty num xx)
  where
  go acc n xs = case n, xs of
    0, _ -> acc
    _, Empty -> acc
    _, Cons h t -> go (h : acc) (n - 1) t

take' :: ∀ x. Int -> List x -> List x
take' n xs =
  if n <= 0 then
    Empty
  else case xs of
    Empty -> Empty
    Cons h t -> h : take' (n - 1) t

-- | Take first `n` elements of the list and return remaining elements
-- | as a result.
-- | ```purescript
-- | drop 2 (1 : 2 : 3 : Empty) = 3 : Empty
-- | drop 9 ('a' : 'b' : 'c' : Empty) = Empty
-- | ```
drop :: ∀ x. Int -> List x -> List x
drop = case _, _ of
  n, xs
    | n <= 0 -> xs
  _, Empty -> Empty
  n, Cons _ t -> drop (n - 1) t

-- | Apply given function to each element of the list
-- | producing a list of results.
-- | 
-- | ```purescript
-- | map (_ + 10) (1 : 3 : 7 : Empty) = 11 : 13 : 17 : Empty
-- | map (_ * 2) Empty = Empty
-- | map identity (1 : 3 : 7 : Empty) = (1 : 3 : 7 : Empty)
-- | ```
map :: ∀ a b. (a -> b) -> List a -> List b
map f xs = go Empty identity $ go Empty f xs
  where
  go :: ∀ x y. List y -> (x -> y) -> List x -> List y
  go acc g = case _ of
    Empty -> acc
    Cons h t -> go (g h : acc) g t

map' :: ∀ a b. (a -> b) -> List a -> List b
map' f = case _ of
  Empty -> Empty
  Cons h t -> Cons (f h) (map f t)

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
filter f = go identity
  where
  go k = case _ of
    Empty -> k Empty
    Cons h t -> go (if f h then (k <<< (h : _)) else k) t
