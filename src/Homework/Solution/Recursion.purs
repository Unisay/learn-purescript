module Homework.Solution.Recursion where

import Prelude

factorialNonTailRecursive :: Int -> Int
factorialNonTailRecursive = case _ of
  1 -> 1
  n -> n * factorialNonTailRecursive (n - 1)

factorialTailRecursive :: Int -> Int
factorialTailRecursive = go 1
  where
  go accum = case _ of
    1 -> accum
    n -> go (accum * n) (n - 1)

factorialTailRecursive' :: ∀ a. Eq a => Ring a => a -> a
factorialTailRecursive' = go one
  where
  go accum = case _ of
    n
      | n == one -> accum
    n -> go (accum * n) (n - one)

fibonacciNonTailRecursive :: Int -> Int
fibonacciNonTailRecursive = case _ of
  0 -> 0
  1 -> 1
  n -> fibonacciNonTailRecursive (n - 1) + fibonacciNonTailRecursive (n - 2)

fibonacciTailRecursive :: Int -> Int
fibonacciTailRecursive = go 1 0
  where
  go a b = case _ of
    0 -> b
    count -> go (a + b) a (count - 1)

isPowerOfTwo :: Int -> String
isPowerOfTwo n =
  if go 1 n then
    "YES"
  else
    "NO"
  where
  go :: Int -> Int -> Boolean
  go a x = case compare x a of
    EQ -> true
    LT -> false
    GT -> go (a * 2) x
