module Applicative.Parsing.Lexer
  ( lexer
  ) where

import Prelude
import Data.Array as A
import Data.String.NonEmpty as NES
import Data.Foldable as F
import Applicative.Parsing.Types (Token(..))
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(Just, Nothing))
import Data.Natural (intToNat)
import Data.Array.NonEmpty as NEA
import Data.String.CodeUnits as String
import Data.Char.Unicode (isSpace)
import Data.String.NonEmpty.CodeUnits (toCharArray)
import Data.Int as Int

type Acc
  = { lexeme :: Array Char, tokens :: Array Token }

lexer :: NES.NonEmptyString -> Array Token
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
                <$> ( NEA.toArray
                      >>> String.fromCharArray
                      >>> Int.fromString
                  )
                    nea

            token = case maybeNum of
              Just number -> Number number
              Nothing ->
                if F.length nea == 1 then
                  Single (NE.head nea)
                else
                  Lexeme nea
          in
            A.snoc acc'.tokens token
  }
