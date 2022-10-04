module FSM where

import Custom.Prelude

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
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

newtype FSMT ∷ Type → (Type → Type) → Type → Type
newtype FSMT i m o = FSMT (i → (Tuple (m o) (FSMT i m o)))

derive instance Newtype (FSMT i m o) _

instance MonadTrans (FSMT i) where
  lift ∷ ∀ m a i. Monad m ⇒ m a → FSMT i m a
  lift m = ?x

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

data RPInput = SwitchForward | SwitchBack | OnOffButton | Pause | StopPlay

type RPState =
  { nowPlaying ∷ Maybe Song, isTurnedOn ∷ Boolean, playlist ∷ Playlist }

type RPOutput = { nowPlaying ∷ Maybe Song, isTurnedOn ∷ Boolean }

type Song = { author ∷ String, songName ∷ String }

type Playlist = NonEmptyArray Song

recordPlayerOn ∷ RecordPlayer
recordPlayerOn = FSMT case _ of
  SwitchForward → do
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
