module Data.BinTree where

import Prelude
import PrettyPrinter
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)

newtype BinTree a
  = BinTree
  { leftBranch :: Maybe (BinTree a)
  , element :: a
  , rightBranch :: Maybe (BinTree a)
  }

derive instance newtypeBinTree :: Newtype (BinTree a) _

instance showBinTree :: Show a => Show (BinTree a) where
  show = layout <<< prettyPrint
    where
    prettyPrint :: BinTree a -> Doc
    prettyPrint bt = case left bt, right bt of
      Just lt, Just rt ->
        el
          <> line
          <> text "┣━ "
          <> (nest 3 $ prettyPrint lt)
          <> one (nest 3 $ prettyPrint rt)
      Just lt, Nothing -> branch bt lt
      Nothing, Just rt -> branch bt rt
      Nothing, Nothing -> el
      where
      branch t b = el <> one (nest 3 $ prettyPrint b)

      one d = line <> text "┗━ " <> d

      el = text (show $ elem bt)

left :: ∀ a. BinTree a -> Maybe (BinTree a)
left = unwrap >>> _.leftBranch

right :: ∀ a. BinTree a -> Maybe (BinTree a)
right = unwrap >>> _.rightBranch

elem :: ∀ a. BinTree a -> a
elem = unwrap >>> _.element

testBTree :: BinTree Int
testBTree =
  tree
    (ltree (rtree 45 (rtree 42 (singleton 43))) 99)
    100
    (tree (singleton 110) 120 (singleton 200))

singleton :: ∀ a. a -> BinTree a
singleton a =
  BinTree
    { leftBranch: Nothing
    , element: a
    , rightBranch: Nothing
    }

tree :: ∀ a. BinTree a -> a -> BinTree a -> BinTree a
tree l e r =
  BinTree
    { leftBranch: Just l
    , element: e
    , rightBranch: Just r
    }

ltree :: ∀ a. BinTree a -> a -> BinTree a
ltree l e =
  BinTree
    { leftBranch: Just l
    , element: e
    , rightBranch: Nothing
    }

rtree :: ∀ a. a -> BinTree a -> BinTree a
rtree e r =
  BinTree
    { leftBranch: Nothing
    , element: e
    , rightBranch: Just r
    }
