module Applicative.Parsing.Parser.Standard where

import Prelude
import Applicative.Parsing.Types (Parser(..), ParsingFunction, Result(..), Token(..))
import Data.Array (uncons)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..), maybe)
import Data.Natural (Natural)
import Data.String (CodePoint)
import Data.String.CodePoints as String

singleParser :: Parser CodePoint -- but returns only Single
singleParser = Parser parsingFunc
  where
  parsingFunc :: ParsingFunction CodePoint
  parsingFunc tokens = { remainder: maybe [] _.tail unconsed, result }
    where
    result = case _.head <$> unconsed of
      -- Single ch -> Ok (Single ch)
      Just (Single ch) -> Ok ch
      Just (Lexeme l) ->
        Err
          $ "Expected any single character but got lexeme: "
          <> show l
      Just (Number n) ->
        Err
          $ "Expected any single character number but got a number: "
          <> show n
      Nothing -> Err "Expected any single character but got EOF"

    unconsed :: Maybe { head :: Token, tail :: Array Token }
    unconsed = uncons tokens

naturalParser :: Parser Natural
naturalParser =
  Parser \tokens -> case uncons tokens of
    Just { head: Number nat, tail } -> { remainder: tail, result: Ok nat }
    _ ->
      { remainder: tokens
      , result: Err "Expected natural number"
      }

keyword :: String -> Parser Unit
keyword expected =
  Parser \tokens ->
    let
      err msg = { remainder: tokens, result: Err msg }
    in
      case uncons tokens of
        Just { head: Lexeme neac, tail } ->
          let
            ok cmd = { result: Ok cmd, remainder: tail }
          in
            case String.fromCodePointArray (NE.toArray neac) of
              s
                | s == expected -> ok unit
              _ -> err $ "Unexpected string, expecting keyword"
        _ -> err $ "Unexpected string, expecting keyword"
