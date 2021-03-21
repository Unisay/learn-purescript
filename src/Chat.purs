module Chat where

import Prelude
import Applicative.Parsing.Parser.Standard (keyword, naturalParser, singleParser)
import Applicative.Parsing.Types (Parser(..), ParsingFunction, Result(..), Token(..))
import Control.Alt ((<|>))
import Data.Array (uncons)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..), maybe, optional)
import Data.Natural (Natural)
import Data.String (codePointFromChar)
import Data.String.CodePoints as String

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

instance showCommand :: Show Command where
  show = case _ of
    Hello user -> "hello " <> show user
    List sex -> "list " <> maybe "" show sex
    Kiss nick -> "kiss " <> show nick
    Kick nick -> "kick " <> show nick
    Quit -> "quit"

data Sex
  = Male
  | Female

instance showSex :: Show Sex where
  show = case _ of
    Male -> "M"
    Female -> "F"

commandParser :: Parser Command
commandParser =
  parserCommandHello
    <|> parserCommandList
    <|> parserCommandKiss
    <|> parserCommandKick
    <|> parserCommandQuit

parserCommandHello :: Parser Command
parserCommandHello = Hello <$ keyword "!hello" <*> userParser

parserCommandList :: Parser Command
parserCommandList = (keyword "!list" $> List) <*> optional sexParser

parserCommandKiss :: Parser Command
parserCommandKiss = (keyword "!kiss" $> Kiss) <*> nicknameParser

parserCommandKick :: Parser Command
parserCommandKick = (keyword "!kick" $> Kick) <*> nicknameParser

parserCommandQuit :: Parser Command
parserCommandQuit = keyword "!quit" $> Quit

-- !hello Chiki M 39 1
userParser :: Parser User
userParser = ado
  nickname <- nicknameParser
  sex <- sexParser
  age <- ageParser
  kids <- kidsParser
  in { nickname, sex, age, kids }

kidsParser :: Parser Natural0
kidsParser = naturalParser

nicknameParser :: Parser Nickname
nicknameParser = Parser parsingFunction
  where
  parsingFunction :: ParsingFunction Nickname
  parsingFunction tokens = case uncons tokens of
    Just { head: Lexeme neac, tail } ->
      { remainder: tail
      , result: Ok ((NE.toArray >>> String.fromCodePointArray) neac)
      }
    Just { head: Single c, tail } ->
      { remainder: tail
      , result: Ok (String.singleton c)
      }
    Just _ ->
      { remainder: tokens
      , result: Err "Nickname must not contain only digits"
      }
    Nothing ->
      { remainder: tokens
      , result: Err "Expected Nickname"
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
  where
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
              cp
                | codePointFromChar 'M' == cp -> Ok Male
              cp
                | codePointFromChar 'F' == cp -> Ok Female
              cp -> Err $ "Expected 'M' | 'F' but got " <> show cp
            Err error -> Err error
      }
