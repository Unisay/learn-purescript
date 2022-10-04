module FSM where

import Custom.Prelude
import Prelude

import Control.Monad.Cont (class MonadTrans, lift)
import Control.Monad.State (State)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (get, state)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Machine.Mealy (MealyT, Step(..), fromArray, mealy, toUnfoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

newtype FSMT ∷ Type → (Type → Type) → Type → Type
newtype FSMT i m o = FSMT (i → (Tuple (m o) (FSMT i m o)))

derive instance Newtype (FSMT i m o) _

instance MonadTrans (FSMT i) where
  lift ∷ ∀ m a. Monad m ⇒ m a → FSMT i m a
  lift m = FSMT \_ → Tuple m (lift m)

{- Задание:

сделать FSMT не бесконечным генератором output-ов а конечным (F = Finite),
т.е. чтоб машина в ответ на любой input могла:
* Сгенерировать output и перейти в следующее состояние; <-- это уже есть
* Сгенерировать output и остановиться (Halt);           <-- а это надо сделать
ближайшим аналогом является `List a`, где `Cons a (List a)` даёт бесконечный список,
а `Nil` делает его конечным.

-}

instance Functor (FSMT i m)
instance Apply (FSMT i m)
instance Applicative (FSMT i m)
instance Bind (FSMT i m)
instance Monad (FSMT i m)

instance MonadState s m ⇒ MonadState s (FSMT i m) where
  state f = lift (state f)

stepFSMT ∷ ∀ i m o. FSMT i m o → i → Tuple (m o) (FSMT i m o)
stepFSMT = unwrap

stepFSM ∷ ∀ i o. FSM i o → i → Tuple o (FSM i o)
stepFSM fsm = unwrap fsm >>> lmap unwrap

type FSM i o = FSMT i Identity o

------------------------------------------------------------------------------

data Input = Push | Coin
data Output = Red | Green

derive instance Eq Output

type Turnstile = FSMT Input Effect Output

turnstileClosed ∷ Turnstile
turnstileClosed = FSMT case _ of
  Push → Tuple (log "push -> red: closed." $> Red) turnstileClosed
  Coin → Tuple (log "coin -> green: opening..." $> Green) turnstileOpen

turnstileOpen ∷ Turnstile
turnstileOpen = FSMT case _ of
  Push → Tuple (log "push -> red: closing... " $> Red) turnstileClosed
  Coin → Tuple (log "coin -> green: returning coin." $> Green) turnstileOpen

runTurnstile ∷ Turnstile → NonEmpty Array Input → Effect (Array Output)
runTurnstile ts inputs = (foldl f { ts, outputs: pure [] } inputs).outputs
  where
  f
    ∷ { ts ∷ Turnstile, outputs ∷ Effect (Array Output) }
    → Input
    → { ts ∷ Turnstile, outputs ∷ Effect (Array Output) }
  f acc input =
    let
      Tuple output nextFsm = stepFSMT acc.ts input
    in
      { ts: nextFsm
      , outputs: Array.snoc <$> acc.outputs <*> output
      }

test ∷ Effect Boolean
test = do
  let inputs = Push :| [ Push, Coin, Coin, Push, Push ]
  outputs ← runTurnstile turnstileClosed inputs
  pure $ outputs == [ Red, Red, Green, Green, Red, Red ]

-- ⏪⏩▶⏸⏹

type RecordPlayer = FSMT RPInput (State RPState) RPOutput

data RPInput = Forward | Rewind | Play | Pause | Stop

type RPState =
  { nowPlaying ∷ Maybe Song
  , isTurnedOn ∷ Boolean
  , playlist ∷ Playlist
  }

type RPOutput = { nowPlaying ∷ Maybe Song, isTurnedOn ∷ Boolean }

type Song = { author ∷ String, songName ∷ String }

type Playlist = NonEmptyArray Song

recordPlayerOn ∷ RecordPlayer
recordPlayerOn = FSMT case _ of
  Forward → do
    { nowPlaying, playlist } ← get
    case nowPlaying of
      Nothing → Tuple (pure { nowPlaying: Nothing, isTurnedOn: true })
        recordPlayerOn
      Just song → Tuple
        ( pure
            { nowPlaying: fromMaybe (NEA.head playlist) $ NEA.index playlist
                (add 1 $ fromMaybe 0 $ NEA.elemIndex song playlist)
            }
        )

type M i o = MealyT Identity i o

m1 ∷ ∀ a. M a Int
m1 = pure 42

m2 ∷ ∀ a. M a Int
m2 = fromArray [ 1, 2, 3, 100 ]

m3 ∷ Int → M Int String
m3 s = mealy \i →
  pure (Emit (show (s + i)) (m3 (s + 1)))

run ∷ ∀ i o. M i o → i → Array o
run m i = toUnfoldable i m
