module Recursion where

import Prelude

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
