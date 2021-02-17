module Applicative.Parsing where

import Prelude
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Array.NonEmpty as NE
import Data.Char.Unicode (isSpace)
import Data.Foldable (length)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Natural (Natural, intToNat)
import Data.String.CodeUnits as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.CodeUnits (toCharArray)

type Nickname
  = String

type Natural0
  = Natural

newtype Natural1
  = Natural1 Natural

type User
  = { nickname :: Nickname
    , sex :: Sex
    , age :: Natural1
    , kids :: Natural0
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

instance showSex :: Show Sex where
  show = case _ of
    Male -> "Male"
    Female -> "Female"

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

lexer :: NonEmptyString -> Array Token
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
                maybeNum =
                  intToNat
                    <$> (toArray >>> String.fromCharArray >>> fromString) nea

                token = case maybeNum of
                  Just number -> Number number
                  Nothing ->
                    if length nea == 1 then
                      Single (NE.head nea)
                    else
                      Lexeme nea
              in
                A.snoc acc'.tokens token
      }

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

nicknameParser :: Parser Nickname
nicknameParser = Parser parsingFunction
  where
  parsingFunction :: ParsingFunction Nickname
  parsingFunction tokens = case NE.uncons tokens of
    { head: Lexeme neac, tail } ->
      { remainder: tail
      , result: Ok ((NE.toArray >>> String.fromCharArray) neac)
      }
    { head: Single c, tail } ->
      { remainder: tail
      , result: Ok (String.singleton c)
      }
    _ ->
      { remainder: NE.toArray tokens
      , result: Err "Nickname must not contain only digits"
      }

naturalParser :: Parser Natural
naturalParser =
  Parser \tokens -> case NE.uncons tokens of
    { head: Number nat, tail } -> { remainder: tail, result: Ok nat }
    _ ->
      { remainder: NE.toArray tokens
      , result: Err "Expected natural number"
      }

-- lexemeParser :: Parser Token -- but returns only Lexeme
-- lexemeParser = ?lexemeParser
singleParser :: Parser Token -- but returns only Single
singleParser = Parser parsingFunc
  where
  parsingFunc :: ParsingFunction Token
  parsingFunc tokens = { remainder: unconsed.tail, result }
    where
    result = case unconsed.head of
      -- Single ch -> Ok (Single ch)
      sch@(Single _) -> Ok sch
      Lexeme l ->
        Err
          $ "Expected any single character but got lexeme: "
          <> show l
      Number n ->
        Err
          $ "Expected any single character number but got a number: "
          <> show n

    unconsed :: { head :: Token, tail :: Array Token }
    unconsed = NE.uncons tokens

-- numberParser :: Parser Number -- but returns only Number
-- numberParser = ?numberParser
sexParser :: Parser Sex
sexParser =
  Parser \newTokens ->
    let
      Parser originalParsingFunction = singleParser

      originalRecord = originalParsingFunction newTokens
    in
      { remainder: originalRecord.remainder
      , result:
          case originalRecord.result of
            Ok single -> case single of
              Single 'M' -> Ok Male
              Single 'F' -> Ok Female
              token -> Err $ "Expected 'M' | 'F' but got " <> show token
            Err error -> Err error
      }

-- sexParser = Parser parsingFunction
--   where
--   parsingFunction :: ParsingFunction Sex
--   parsingFunction tokens =
--     { remainder: unconsed.tail
--     , result:
--         case unconsed.head of
--           Single 'M' -> Ok Male
--           Single 'F' -> Ok Female
--           token -> Err $ "Expected 'M' | 'F' but got " <> show token
--     }
--     where
--     unconsed :: { head :: Token, tail :: Array Token }
--     unconsed = NE.uncons tokens
parseStr :: forall r. Parser r -> String -> Result r
parseStr (Parser parsingFunction) input = case NES.fromString input of
  Nothing -> Err "Input is empty"
  Just nonEmptyInput -> case NE.fromArray (lexer nonEmptyInput) of
    Nothing -> Err "No valid input received!"
    Just tokens -> (parsingFunction tokens).result
