module Chat where

import Prelude
import Applicative.Parsing.Parser.Standard (naturalParser, singleParser)
import Applicative.Parsing.Types (Parser(..), ParsingFunction, Result(..), Token(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe)
import Data.Natural (Natural)
import Data.String.CodeUnits as String

type Nickname
  = String

type Natural0
  = Natural

newtype Natural1
  = Natural1 Natural

instance showNatural1 :: Show Natural1 where
  show (Natural1 x) = show x

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

kidsParser :: Parser Natural0
kidsParser = naturalParser

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

ageParser :: Parser Natural1
ageParser =
  Parser \newTokens ->
    let
      Parser originalParsingFunction = naturalParser

      originalRecord = originalParsingFunction newTokens
    in
      { remainder: originalRecord.remainder
      , result:
          nat0ToResNat1 originalRecord.result
      }

nat0ToResNat1 :: Result Natural0 -> Result Natural1
nat0ToResNat1 = case _ of
  Err e -> Err e
  Ok x ->
    if x == zero then
      Err "Expected non-zero nat"
    else
      Ok $ Natural1 x

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
              'M' -> Ok Male
              'F' -> Ok Female
              ch -> Err $ "Expected 'M' | 'F' but got " <> show ch
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
