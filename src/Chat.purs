module Chat where

import Prelude
import Applicative.Parsing.Parser.Standard (keyword, naturalParser, singleParser)
import Applicative.Parsing.Types (Parser(..), ParsingFunction, Result(..), Token(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (uncons)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..), maybe, optional)
import Data.Natural (Natural)
import Data.Semigroup.Foldable (class Foldable1, foldr1)
import Data.String (codePointFromChar)
import Data.String.CodePoints as String
import Homework.Todo (todo)

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
  choice
    $ NE.cons' parserCommandHello
        [ parserCommandKiss
        , parserCommandKick
        , parserCommandQuit
        , parserCommandList
        ]
  where
  -- Example using ado notation
  parserCommandHello :: Parser Command
  parserCommandHello = ado
    keyword "!hello"
    user <- userParser
    in Hello user

  -- Example using `apply` operator
  parserCommandList :: Parser Command
  parserCommandList = (keyword "!list" $> List) <*> optional sexParser

  -- Example using `applyRight` operator
  parserCommandKiss :: Parser Command
  parserCommandKiss = keyword "!kiss" *> map Kiss nicknameParser

  -- Example using `applyLeft` operator
  parserCommandKick :: Parser Command
  parserCommandKick = map Kick nicknameParser <* keyword "!kick"

  parserCommandQuit :: Parser Command
  parserCommandQuit = keyword "!quit" $> Quit

-- | Homework: implement choice 
-- choice [p1] == p1
-- choice [p1, p2, p3] == p1 <|> p2 <|> p3
-- choice [p1, p2, p3, p4] == p1 <|> p2 <|> p3 <|> p4
-- forall a. (a -> a -> a) -> t a -> a
choice :: forall t x. Foldable1 t => t (Parser x) -> Parser x
choice = foldr1 (<|>)

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
ageParser = do
  nat0 <- naturalParser
  if nat0 == zero then
    throwError "Expected non-zero nat"
  else
    pure $ Natural1 nat0

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

-- sequence [keyword "A" $> 1, keyword "B" $> 42, keyword "C" $> 119] `parseStr` "A B C" OK: [1,42,119]
-- sequence :: forall a. Array (Parser a) -> Parser (Array a)
-- sequence = todo "pls implement"
