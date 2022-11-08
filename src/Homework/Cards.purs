module Homework.Cards where

import Custom.Prelude

import Control.Coroutine
  ( Consumer
  , Coroutine(..)
  , Producer
  , Transducer
  , await
  , liftStateless
  , runPipeline
  , transduceProducer
  )
import Control.Coroutine.Suspension.Functor (Consume(..), produce)
import Control.Monad.State (StateT, get, runStateT)
import Control.Monad.State.Trans (modify_)
import Data.Array (modifyAt, (:))
import Data.Array as Array
import Data.Enum (toEnum)
import Data.String.CodePoints (CodePoint, fromCodePointArray)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)

type Card = CodePoint

type PlayerM = StateT (Array PlayerData) Aff

type Generator = Producer Int PlayerM Unit

type Dealer m = Transducer Int Card m Unit

type PlayerData = { name ∷ String, hand ∷ Array Card }

type Player = Consumer (Maybe Card) PlayerM Unit

main ∷ Effect Unit
main = launchAff_ do
  void $ runStateT (runPipeline (transduceProducer generator dealer) player)
    playersArray

generator ∷ Generator
generator = Coroutine \_ → do
  int ← liftEffect $ randomInt 1 36
  pure $ Left $ produce int generator

dealer ∷ ∀ m. Monad m ⇒ Dealer m
dealer = liftStateless (cardFromInt >>> Array.fromFoldable)

player ∷ Player
player = await >>= processInput
  where
  processInput = maybe pass \card → do
    arrOfData ← get
    emptyHandIdx arrOfData # maybe (showPlayersHand arrOfData)
      \idx → Coroutine \_ → pure (Left (Consume processInput)) <$>
        modify_ \oldState → fromMaybe oldState
          $ modifyAt idx (\st@{ hand } → st { hand = card : hand }) arrOfData

  emptyHandIdx = Array.findIndex \{ hand } → Array.length hand < handCapacity

startingIndex ∷ Int
startingIndex = 0x1F0A0

cardFromInt ∷ Int → Maybe Card
cardFromInt i
  | i >= 1 && i <= 36 = toEnum (startingIndex + i)
  | otherwise = Nothing

handCapacity ∷ Int
handCapacity = 6

showPlayersHand ∷ ∀ m. MonadEffect m ⇒ Array PlayerData → m Unit
showPlayersHand =
  traverse_ \{ name, hand } → log $ "Player " <> name <> ": " <>
    (fromCodePointArray hand)

playersArray ∷ Array PlayerData
playersArray =
  [ { name: "Vadym"
    , hand: []
    }
  , { name: "Unisay"
    , hand: []
    }
  , { name: "Andrew"
    , hand: []
    }
  ]
