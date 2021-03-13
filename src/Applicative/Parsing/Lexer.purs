module Applicative.Parsing.Lexer
  ( lexer
  ) where

import Prelude
import Applicative.Parsing.Types (Token(..))
import Data.Array as A
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode (isSpace)
import Data.Foldable as F
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Natural (intToNat)
import Data.String (CodePoint)
import Data.String.CodePoints as String
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.CodePoints (toCodePointArray)

type Acc
  = { lexeme :: Array CodePoint, tokens :: Array Token }

lexer :: NES.NonEmptyString -> Array Token
lexer s = (go initialAcc (toCodePointArray s)).tokens
  where
  initialAcc :: Acc
  initialAcc = { lexeme: [], tokens: [] }

  go :: Acc -> Array CodePoint -> Acc
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
                      >>> String.fromCodePointArray
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
