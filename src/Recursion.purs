module Recursion where

import Prelude hiding (map)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr, unfoldr1)
import Homework.Todo (todo, todo')
import Unsafe.Coerce (unsafeCoerce)

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

ints :: Int -> List Int
ints size = unfoldr f 1
  where
  f :: Int -> Maybe (Tuple Int Int)
  f seed =
    if seed > size then
      Nothing
    else
      Just $ Tuple seed (seed + 1)

infts :: List Int
infts = unfoldr f 1
  where
  f :: Int -> Maybe (Tuple Int Int)
  f seed = Just $ Tuple seed (seed + 1)

-- bools 0 == Empty
-- bools 1 == true : Empty
-- bools 2 == false : true : Empty
-- bools 3 == true : false : true : Empty
-- bools 4 == false : true : false : true : Empty
bools :: Int -> List Boolean
bools size = unfoldr f (Tuple false size)
  where
  f :: Tuple Boolean Int -> Maybe (Tuple Boolean (Tuple Boolean Int))
  f (Tuple prevBool remainedCount) =
    if remainedCount < 1 then
      Nothing
    else
      let
        nextBool = not prevBool
      in
        Just $ Tuple nextBool (Tuple nextBool (remainedCount - 1))

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
headOr a = todo' "please implement"

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
sum = todo' "please implement"

-- | Multiply all elements of the list
-- | ```purescript
-- | product (2 : 2 : Empty) = 4
-- | product (1.0 : 2.0 : 3.0 : Empty) = 6.0
-- | ```
product :: ∀ x. Semiring x => List x -> x
product = todo' "please implement"

-- | Reverse elements of the list
-- | ```purescript
-- | reverse (1 : 2 : 3 : Empty) = (3 : 2 : 1 : Empty) 
-- | ```
-- | Law: 
-- | ```purescript
-- | ∀ xs. xs = reverse (reverse xs) 
-- | ```
reverse :: ∀ x. List x -> List x
reverse xs = todo "please implement"

-- | Take first `n` elements of the list and return them as a result.
-- | ```purescript
-- | take 2 (1 : 2 : 3 : Empty) = 1 : 2 : Empty
-- | take 9 (1 : 2 : 3 : Empty) = 1 : 2 : 3 : Empty
-- | ```
take :: ∀ x. Int -> List x -> List x
take n xs = todo "please implement"

-- | Take first `n` elements of the list and return remaining elements
-- | as a result.
-- | ```purescript
-- | drop 2 (1 : 2 : 3 : Empty) = 3 : Empty
-- | drop 9 (1 : 2 : 3 : Empty) = Empty
-- | ```
drop :: ∀ x. Int -> List x -> List x
drop n xs = todo "please implement"

-- | Apply given function to each element of the list
-- | producing a list of results.
-- | 
-- | ```purescript
-- | map (_ + 10) (1 : 3 : 7 : Empty) = 11 : 13 : 17 : Empty
-- | map (_ * 2) Empty = Empty
-- | map identity (1 : 3 : 7 : Empty) = (1 : 3 : 7 : Empty)
-- | ```
map :: ∀ a b. (a -> b) -> List a -> List b
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
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter xs = todo "please implement"

instance foldableList :: Foldable List where
  foldl :: forall a acc. (acc -> a -> acc) -> acc -> List a -> acc
  foldl f acc = case _ of
    Empty -> acc
    Cons h t -> foldl f (f acc h) t
  foldr :: forall a acc. (a -> acc -> acc) -> acc -> List a -> acc
  foldr f acc = case _ of
    Empty -> acc
    Cons h t -> f h (foldr f acc t)
  foldMap :: forall a m. Monoid m => (a -> m) -> List a -> m
  foldMap = unsafeCoerce unit

instance unfoldable1List :: Unfoldable1 List where
  unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> List a
  unfoldr1 k seed = case k seed of
    Tuple a Nothing -> Cons a Empty
    Tuple a (Just newSeed) -> Cons a (unfoldr1 k newSeed)

instance unfoldableList :: Unfoldable List where
  unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> List a
  unfoldr k seed = case k seed of
    Nothing -> Empty
    Just (Tuple a newSeed) -> Cons a (unfoldr k newSeed)
