module Example where

import Prelude

data Handy
  = Nokia3110
  | SamsungGalaxyS

type Color
  = String

type Size
  = Int

type Age
  = Int

data Case
  = MakeNewCase Color Size Age

colorToString :: Color -> String
colorToString color = color

increment :: Int -> Int
increment x = x + 1

decrement :: Int -> Int
decrement x = x - 1

ignore :: ∀ a. a -> Int
ignore _ = 0

ignoreChar :: Char -> Int
ignoreChar _ = 500

modify42 :: (Int -> Int) -> Int
modify42 operation = operation 42

selectModifier :: Handy -> (Int -> Int)
selectModifier = case _ of
  Nokia3110 -> increment
  SamsungGalaxyS -> decrement
