module Recursion where

import Prelude
import Debug.Trace (spy)
import Homework.Todo (todo, todo')

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
  show = render

render :: forall a. Show a => List a -> String
render a =
  parens
    ( case a of
        Empty -> ""
        Cons h t -> show h <> " : " <> render t
    )
  where
  parens :: String -> String
  parens x = "(" <> x <> ")"

infixr 6 Cons as :

ints :: Int -> List Int
ints size = go size Empty
  where
  go :: Int -> List Int -> List Int
  go remainder acc = case remainder of
    0 -> acc
    n -> go (remainder - 1) (remainder : acc)

length :: forall a. List a -> Int
length = case _ of
  Empty -> 0
  Cons h t -> 1 + length t

length' :: forall a. List a -> Int
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
headOr :: forall a. a -> List a -> a
headOr a = case _ of
  Empty -> a
  Cons h _ -> h

-- | Sum all elements of the list
-- | ```purescript
-- | sum (1 : 2 : 3 : Empty) = 6
-- | sum (1.0 : 2.0 : 3.0 : Empty) = 6.0
-- | ```
sum ::
  forall x.
  Semiring x => -- | Semiring gives you `zero`, `one` and can `add` and `mul`
  List x -> -- List of `add`-able things
  x -- result of summing up (via `add`) all the elements in that list.
sum = todo' "please implement"

-- | Multiply all elements of the list
-- | ```purescript
-- | product (2 : 2 : Empty) = 4
-- | product (1.0 : 2.0 : 3.0 : Empty) = 6.0
-- | ```
product :: forall x. Semiring x => List x -> x
product = todo' "please implement"

-- | Take first `n` elements of the list and return them as a result.
-- | ```purescript
-- | take 2 (1 : 2 : 3 : Empty) = 1 : 2 : Empty
-- | take 9 (1 : 2 : 3 : Empty) = 1 : 2 : 3 : Empty
-- | ```
take :: forall x. Int -> List x -> List x
take n xs = todo "please implement"

-- | Take first `n` elements of the list and return remaining elements
-- | as a result.
-- | ```purescript
-- | drop 2 (1 : 2 : 3 : Empty) = 3 : Empty
-- | drop 9 (1 : 2 : 3 : Empty) = Empty
-- | ```
drop :: forall x. Int -> List x -> List x
drop n xs = todo "please implement"

-- | Reverse elements of the list
-- | ```purescript
-- | reverse (1 : 2 : 3 : Empty) = (3 : 2 : 1 : Empty) 
-- | ```
-- | Law: 
-- | ```purescript
-- | forall xs. xs = reverse (reverse xs) 
-- | ```
reverse :: forall x. List x -> List x
reverse xs = todo "please implement"

-- | Apply given function to each element of the list
-- | producing a list of results.
-- | 
-- | ```purescript
-- | map (_ + 10) (1 : 3 : 7 : Empty) = 11 : 13 : 17 : Empty
-- | map (_ * 2) Empty = Empty
-- | map identity (1 : 3 : 7 : Empty) = (1 : 3 : 7 : Empty)
-- | ```
map :: forall a b. (a -> b) -> List a -> List b
map as = todo "please implement"

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
filter :: forall a. (a -> Boolean) -> List a -> List a
filter xs = todo "please implement"
