module App.Web.Dialog.Main where

import Prelude
import App.Web.Dialog.Foreign (tell, ask)
import Data.Array (replicate)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Natural (Natural, intToNat, natToInt)
import Data.String (joinWith)
import Effect (Effect)

main :: Effect Unit
main = do
  tell "Диалог начался."
  name <- ask "Как тебя зовут?"
  answer <- askAge
  case answer of
    Just years -> do
      let
        days = years * daysPerYear
      tell $ name <> ", ты прожил уже " <> show days <> " дней!"
      tell $ lifespan days
    _ -> tell "Я не могу сказать тебе сколько ты прожил дней."
  where
  askAge :: Effect (Maybe Natural)
  askAge = parseAge <$> ask "Сколько тебе полных лет?"
    where
    parseAge :: String -> Maybe Natural
    parseAge str = case Int.fromString str of
      Just i
        | i >= 0 -> Just (intToNat i)
      _ -> Nothing

  daysPerYear :: Natural
  daysPerYear = intToNat 365

  lifespan :: Natural -> String
  lifespan days = joinWith " " $ replicate (natToInt days) "⬜"
