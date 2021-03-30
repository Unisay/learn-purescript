module Applicative.Parsing.Types where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Natural (Natural)
import Data.String (CodePoint)

data Token
  = Lexeme (NonEmptyArray CodePoint)
  | Single CodePoint
  | Number Natural

instance showToken :: Show Token where
  show = case _ of
    Lexeme neac -> "Lexeme " <> show neac
    Single chr -> "Single " <> show chr
    Number nat -> "Number " <> show nat

type ParsingFunction r
  = Array Token ->
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

instance applyParser :: Apply Parser where
  apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  apply (Parser pfab) (Parser pfa) = Parser pfb
    where
    pfb neat = case pfab neat of
      record1 -> case pfa record1.remainder of
        { result, remainder } ->
          { remainder
          , result:
              case record1.result, result of
                Err e1, Err e2 -> Err (e1 <> "; " <> e2)
                Err e, Ok _ -> Err e
                Ok _, Err e -> Err e
                Ok ab, Ok a -> Ok (ab a)
          }

instance applicativeParser :: Applicative Parser where
  pure :: forall a. a -> Parser a
  pure x = Parser \remainder -> { result: Ok x, remainder }

instance parserAlt :: Alt Parser where
  alt :: forall a. Parser a -> Parser a -> Parser a
  alt (Parser pf1) (Parser pf2) =
    Parser \tokens ->
      let
        pfr1 = pf1 tokens
      in
        case pfr1.result of
          Ok _ -> pfr1
          Err _ -> pf2 tokens

instance alternativeParser :: Alternative Parser

instance plusParser :: Plus Parser where
  empty :: forall a. Parser a
  empty = Parser \tokens -> { result: Err "", remainder: tokens }
