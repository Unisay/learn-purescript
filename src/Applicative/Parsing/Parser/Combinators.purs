module Applicative.Parsing.Parser.Combinators where

import Applicative.Parsing.Types
import Prelude
import Control.Apply (lift2)
import Data.Array (foldr, (:))

-- Homework:
-- 1. Finish implementation (replace typed hole)
-- 2. Make function type more general (polymorphic)
sequence :: forall a. Array (Parser a) -> Parser (Array a)
sequence = foldr (?acc) (pure [])

-- Homework: 
-- 1. implement traverse directly
-- 2. implement traverse via sequence
-- 3. implement sequence via traverse
traverse :: forall a b. (a -> b) -> Array (Parser a) -> Parser (Array b)
traverse = ?implement
