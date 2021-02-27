module Applicative.Parsing.Types where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Natural (Natural)

data Token
  = Lexeme (NonEmptyArray Char)
  | Single Char
  | Number Natural

instance showToken :: Show Token where
  show = case _ of
    Lexeme neac -> "Lexeme " <> show neac
    Single chr -> "Single " <> show chr
    Number nat -> "Number " <> show nat

type ParsingFunction r
  = NonEmptyArray Token ->
    { remainder :: Array Token
    , result :: Result r
    }

data Result a
  = Ok a
  | Err String

instance functorResult :: Functor Result where
  map f = case _ of
    Ok a -> Ok (f a)
    Err s -> Err s

instance showResult :: Show a => Show (Result a) where
  show = case _ of
    Ok (x :: a) -> "OK: " <> show x
    Err s -> "Error: " <> s

newtype Parser r
  = Parser (ParsingFunction r)

instance functorParser :: Functor Parser where
  map :: forall a b. (a -> b) -> Parser a -> Parser b
  map f (Parser originalParsingFunction) =
    Parser \newTokens ->
      let
        originalRecord = originalParsingFunction newTokens
      in
        { remainder: originalRecord.remainder
        , result: f <$> originalRecord.result
        }
