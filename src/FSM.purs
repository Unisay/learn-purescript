module FSM where

import Custom.Prelude

import Data.Array as Array
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

stepFSMT ∷ ∀ i m o. FSMT i m o → i → Tuple (m o) (FSMT i m o)
stepFSMT = unwrap

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
