module Applicative.Parsing.Parser.Standard where

import Prelude
import Applicative.Parsing.Types (Parser(..), ParsingFunction, Result(..), Token(..))
import Data.Array.NonEmpty as NE
import Data.Natural (Natural)
import Data.String (CodePoint)
import Data.String.CodePoints as String

-- lexemeParser :: Parser Token -- but returns only Lexeme
-- lexemeParser = ?lexemeParser
singleParser :: Parser CodePoint -- but returns only Single
singleParser = Parser parsingFunc
  where
  parsingFunc :: ParsingFunction CodePoint
  parsingFunc tokens = { remainder: unconsed.tail, result }
    where
    result = case unconsed.head of
      -- Single ch -> Ok (Single ch)
      Single ch -> Ok ch
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

naturalParser :: Parser Natural
naturalParser =
  Parser \tokens -> case NE.uncons tokens of
    { head: Number nat, tail } -> { remainder: tail, result: Ok nat }
    _ ->
      { remainder: NE.toArray tokens
      , result: Err "Expected natural number"
      }

keyword :: String -> Parser String
keyword expected =
  Parser \tokens ->
    let
      err msg = { remainder: NE.toArray tokens, result: Err msg }
    in
      case NE.uncons tokens of
        { head: Lexeme neac, tail } ->
          let
            ok cmd = { result: Ok cmd, remainder: tail }
          in
            case String.fromCodePointArray (NE.toArray neac) of
              s
                | s == expected -> ok s
              _ -> err $ "Unexpected string, expecting: " <> expected
        _ -> err $ "Unexpected string, expecting: " <> expected
