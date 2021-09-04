module TwoSum where

import Prelude
import Applicative (lift2)
import Data.Array (head, mapWithIndex, sort, tail)
import Data.HashMap (HashMap, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Foldable as F

{-
https://leetcode.com/problems/two-sum/

Given an array of integers nums and an integer target, 
return indices of the two numbers such that they add up to target.
You may assume that each input would have exactly one solution, 
and you may not use the same element twice.
You can return the answer in any order. 

-}
twoSum :: Array Int -> Int -> Maybe (Array Int)
twoSum items sum = twoSumMap items sum 0 empty

twoSumMap :: Array Int -> Int -> Int -> HashMap Int Int -> Maybe (Array Int)
twoSumMap items sum i m = case head items of
  Just e1 -> case lookup (sum - e1) m of
    Just i2 -> Just [ i, i2 ]
    Nothing -> case tail items of
      Just tailItems -> twoSumMap tailItems sum (i + 1) (insert e1 i m)
      Nothing -> Nothing
  Nothing -> Nothing

twoSum' :: Array Int -> Int -> Maybe (Array Int)
twoSum' xs s = Additive s `F.lookup` lift2 (<>) ixs ixs
  where
  ixs = mapWithIndex (\i x -> Tuple (Additive x) [ i ]) xs

main :: Effect Unit
main = case twoSum' [ 4, 6, -1, 18, -13, 11 ] 5 of
  Just r -> logShow $ sort r
  Nothing -> log "nothing! nada!"
