module Applicative.Parsing where

import Prelude
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Char.Unicode (isSpace)
import Data.Foldable (length)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Natural (Natural)
import Data.String.CodeUnits (toCharArray)

-- Domain:
type Nickname
  = String

type User
  = { nickname :: Nickname
    , sex :: Sex
    , age :: Natural
    , kids :: Natural
    }

data Command
  = Hello User
  | List (Maybe Sex)
  | Kiss Nickname
  | Kick Nickname
  | Quit

data Sex
  = Male
  | Female

-- Parsing:
--                      /hello Yura M 38 2
-- /hello Chiki M 39 1
data Token
  = Lexeme (NonEmptyArray Char)
  | Single Char
  | Number Natural

instance showToken :: Show Token where
  show = case _ of
    Lexeme neac -> "Lexeme " <> show neac
    Single chr -> "Single " <> show chr
    Number nat -> "Number " <> show nat

type Acc
  = { lexeme :: Array Char, tokens :: Array Token }

lexer :: String -> Array Token
lexer s = (go initialAcc (toCharArray s)).tokens
  where
  initialAcc :: Acc
  initialAcc = { lexeme: [], tokens: [] }

  go :: Acc -> Array Char -> Acc
  go prev arr = case A.uncons arr of
    Nothing -> flush prev
    Just { head, tail }
      | isSpace head -> go (flush prev) tail
    Just { head, tail } ->
      go
        { lexeme: A.snoc prev.lexeme head
        , tokens: prev.tokens
        }
        tail
    where
    flush :: Acc -> Acc
    flush acc' =
      { lexeme: []
      , tokens:
          case NE.fromArray acc'.lexeme of
            Nothing -> acc'.tokens
            Just nea ->
              let
                token =
                  if length nea == 1 then
                    Single (NE.head nea)
                  else
                    Lexeme nea
              in
                A.snoc acc'.tokens token
      }

type Input
  = Array Token

type ParsingFunction r
  = Input -> { remainder :: Input, result :: Result r }

data Result a
  = Ok a
  | Err String

newtype Parser r
  = Parser (ParsingFunction r)
