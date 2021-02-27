module Applicative.Parsing where

import Data.Maybe (Maybe(..))
import Applicative.Parsing.Lexer (lexer)
import Applicative.Parsing.Types (Parser(..), Result(..))
import Data.Array.NonEmpty as NE
import Data.String.NonEmpty as NES

-- Parsing:
--                      /hello Yura M 38 2
-- /hello Chiki M 39 1
parseStr :: forall r. Parser r -> String -> Result r
parseStr (Parser parsingFunction) input = case NES.fromString input of
  Nothing -> Err "Input is empty"
  Just nonEmptyInput -> case NE.fromArray (lexer nonEmptyInput) of
    Nothing -> Err "No valid input received!"
    Just tokens -> (parsingFunction tokens).result
