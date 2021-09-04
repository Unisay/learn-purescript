module Applicative.Parsing.Parser.Combinators where

import Prelude
import Applicative.Parsing.Types (Parser)
import Data.Array (foldr)
import Homework.Todo (todo)

-- Homework:
-- 1. Finish implementation (replace typed hole)
-- 2. Make function type more general (polymorphic)
sequence :: forall a. Array (Parser a) -> Parser (Array a)
sequence = foldr (todo "implement") (pure [])

-- Homework: 
-- 1. implement traverse directly
-- 2. implement traverse via sequence
-- 3. implement sequence via traverse
traverse :: forall a b. (a -> b) -> Array (Parser a) -> Parser (Array b)
traverse _ _ = todo "implement"
