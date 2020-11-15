module Example where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

{-

Типы алгебраические:
— Суммы (Sum, +)
— Продукты (Product, *)

data Either x y
  = Left x
  | Right y

data Tuple x y
  = Tuple x y


-}
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

selectModifier :: Tuple Handy Int -> Int
selectModifier (Tuple handy int) = case handy of
  Nokia3110 -> increment int
  SamsungGalaxyS -> decrement int

data Game
  = Mobile
  | PC

instance showGame :: Show Game where
  show Mobile = "Mobile"
  show PC = "PC"

data Price
  = Cheap
  | Expensive

derive instance genericPrice :: Generic Price _

instance showPrice :: Show Price where
  show = genericShow

shouldBuy :: Tuple Game Price -> Boolean
shouldBuy tup = case tup of
  Tuple Mobile Cheap -> false
  Tuple Mobile Expensive -> false
  Tuple PC Cheap -> true
  Tuple PC Expensive -> true

shouldBuy' :: Game -> Price -> Boolean
shouldBuy' game price = case game, price of
  Mobile, Cheap -> false
  Mobile, Expensive -> false
  PC, Cheap -> true
  PC, Expensive -> true

-- swap' :: forall a b. Tuple a b -> Tuple b a
-- swap' = ?homework1
-- flip' :: forall a b c. (a -> b -> c) -> (b -> a -> c)
-- flip' = ?homework2
