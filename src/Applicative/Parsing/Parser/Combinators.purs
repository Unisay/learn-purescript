module Applicative.Parsing.Parser.Combinators where

import Applicative.Parsing.Types
import Prelude
import Control.Apply (lift2)
import Data.Array (foldr, (:))

sequence :: forall a. Array (Parser a) -> Parser (Array a)
sequence = foldr (?acc) (pure [])
