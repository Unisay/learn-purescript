module Chat where

import Prelude
import Applicative.Parsing.Parser.Standard (keyword, naturalParser, singleParser)
import Applicative.Parsing.Types (Parser(..), ParsingFunction, Result(..), Token(..))
import Control.Alt ((<|>))
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe)
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

data Command'
  = Hello'
  | List'
  | Kiss'
  | Quit'

instance showCommandPrime :: Show Command' where
  show = case _ of
    Hello' -> "Hello'"
    List' -> "List'"
    Kiss' -> "Kiss'"
    Quit' -> "Quit'"

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

commandPrimeParser :: Parser Command'
commandPrimeParser =
  Hello' `for` "hello"
    <|> List' `for` "list"
    <|> Kiss' `for` "kiss"
    <|> Quit' `for` "quit"
  where
  -- homework: rewrite without for, use <$   or $>
  for cmd kw = map (const cmd) (keyword kw)

-- Parser \tokens ->
--   let
--     err msg = { remainder: NE.toArray tokens, result: Err msg }
--   in
--     case NE.uncons tokens of
--       { head: Lexeme neac, tail } ->
--         let
--           ok cmd = { result: Ok cmd, remainder: tail }
--         in
--           if NE.head neac /= codePointFromChar '/' then
--             { remainder: NE.toArray tokens
--             , result: Err "Invalid command: all commands start with /"
--             }
--           else case String.fromCodePointArray (NE.tail neac) of
--             "hello" -> ok Hello'
--             "list" -> ok List'
--             "kiss" -> ok Kiss'
--             "quit" -> ok Quit'
--             _ -> err "Unknown command: hello | list | kiss | quit !"
--       _ -> err "Malformed command!"
-- commandParser :: Parser Command
-- commandParser = ?commandParser
-- slashParser :: Parser 
-- Parsing:
--                      /hello Yura M 38 2
-- /hello Chiki M 39 1
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
  parsingFunction tokens = case NE.uncons tokens of
    { head: Lexeme neac, tail } ->
      { remainder: tail
      , result: Ok ((NE.toArray >>> String.fromCodePointArray) neac)
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
