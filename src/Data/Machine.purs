module Data.Machine
  ( Machine
  , StepWithState
  , machine
  , emit
  , emit_
  , halt
  , step
  , mkStep
  , unStep

  -- Construction
  , fromEffect
  , fromFoldable
  , liftMachine

  -- Combination
  , sequentially
  , loop

  -- Destruction
  , runMachine
  , stepMachine

  -- Examples
  , showSink
  , source
  , main
  ) where

import Custom.Prelude hiding (compose)

import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Rec.Class as R
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldr)
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Random (randomInt)

newtype Machine m input output = Machine (input → m (Step m input output))

machine ∷ ∀ m i o. (i → m (Step m i o)) → Machine m i o
machine = Machine

data StepWithState m input output state
  = Halt
  | Emit output state (state → Machine m input output)

halt ∷ ∀ i m o. Step m i o
halt = mkStep Halt

emit ∷ ∀ i m o s. o → s → (s → Machine m i o) → Step m i o
emit o s t = mkStep $ Emit o s t

emit_ ∷ ∀ i m o. o → Machine m i o → Step m i o
emit_ o m = mkStep $ Emit o unit \_ → m

type Step m i o = Exists (StepWithState m i o)

mkStep ∷ ∀ i m o s. StepWithState m i o s → Step m i o
mkStep = mkExists

unStep ∷ ∀ i m o r. (∀ s. StepWithState m i o s → r) → Step m i o → r
unStep = runExists

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------

instance Monad m ⇒ Semigroup (Machine m i o) where
  append = sequentially

instance Monad m ⇒ Semigroupoid (Machine m) where
  compose (Machine bc) (Machine ab) = machine \a →
    ab a >>= step (pure halt) \b mab →
      bc b <#> step halt \c mbc → emit_ c (mbc <<< mab)

instance Lazy (Machine m i o) where
  defer th = machine \i → let Machine m = th unit in m i

--------------------------------------------------------------------------------
-- Type aliases ----------------------------------------------------------------

type PureMachine input output = Machine Identity input output

type Sink m i = Machine m i Unit

type Source m o = Machine m Unit o

--------------------------------------------------------------------------------
-- Construction ----------------------------------------------------------------

fromEffect ∷ ∀ i m o. Applicative m ⇒ m o → Machine m i o
fromEffect = liftMachine <<< const

fromFoldable ∷ ∀ f i m o. Monad m ⇒ Foldable f ⇒ f o → Machine m i o
fromFoldable fo = machine \_ → foldr f (pure halt) fo
  where
  f ∷ o → m (Step m i o) → m (Step m i o)
  f o = map (emit_ o <<< machine <<< const <<< pure)

liftMachine ∷ ∀ i m o. Applicative m ⇒ (i → m o) → Machine m i o
liftMachine f = machine \i → f i <#> flip emit_ (machine \_ → pure halt)

--------------------------------------------------------------------------------
-- Combinators -----------------------------------------------------------------

sequentially ∷ ∀ m i o. Monad m ⇒ Machine m i o → Machine m i o → Machine m i o
sequentially l r =
  machine \i →
    stepMachine l i >>=
      step (stepMachine r i) \o m → pure (emit_ o (sequentially m r))

loop ∷ ∀ m i o. Monad m ⇒ Machine m i o → Machine m i o
loop m = innerLoop m
  where
  innerLoop ∷ Machine m i o → Machine m i o
  innerLoop n = machine \i →
    stepMachine n i >>= step
      (stepMachine (loop m) i)
      \o m' → pure (emit_ o (innerLoop m'))

--------------------------------------------------------------------------------
-- Destruction -----------------------------------------------------------------

runMachine ∷ ∀ m. MonadRec m ⇒ Machine m Unit Unit → m Unit
runMachine = R.tailRecM \(Machine m) →
  m unit <#> step (R.Done unit) \_u → R.Loop

stepMachine ∷ ∀ m i o. Monad m ⇒ Machine m i o → i → m (Step m i o)
stepMachine (Machine m) i = m i

--------------------------------------------------------------------------------
-- Convenience functions -------------------------------------------------------

step ∷ ∀ i m o r. r → (o → (Machine m i o) → r) → Step m i o → r
step h e = unStep case _ of
  Halt → h
  Emit o s k → e o (k s)

--------------------------------------------------------------------------------
-- Example ---------------------------------------------------------------------

type Coins = Int

type Hours = Int

hoursToCoins ∷ Hours → Coins
hoursToCoins = identity

data Input = PutInTicket Hours | PutInCoins Coins

data Output = Display String | Change Coins | ValidatedTicket

data State = WaitingForTicket | WaitingForCoins Coins

parkingMachineAwaitsTicket ∷ PureMachine Input Output
parkingMachineAwaitsTicket = Machine \input → pure case input of
  PutInCoins _coins →
    mkStep $ Emit output nextState nextMachine
    where
    output = Display "Please collect your coins and insert your ticket first."
    nextState = WaitingForTicket
    nextMachine _currentState = parkingMachineAwaitsTicket
  PutInTicket hours →
    mkStep $ Emit output nextState nextMachine
    where
    coins = hoursToCoins hours
    output = Display $ "Please put " <> show coins <> " coins in."
    nextState = WaitingForCoins coins
    nextMachine currentState = case currentState of
      WaitingForTicket → parkingMachineAwaitsTicket
      WaitingForCoins remainingCoins → parkingMachineAwaitsCoins remainingCoins

parkingMachineAwaitsCoins ∷ Coins → PureMachine Input Output
parkingMachineAwaitsCoins remainingCoins = Machine \input → pure case input of
  PutInTicket _hours →
    mkStep $ Emit output nextState nextMachine
    where
    output = Display $ "Please don't put more than 1 ticket!"
    nextState = WaitingForCoins remainingCoins
    nextMachine _currentState = parkingMachineAwaitsCoins remainingCoins
  PutInCoins coins →
    mkStep $ Emit output nextState nextMachine
    where
    remainingCoins' = remainingCoins - coins
    output =
      case compare remainingCoins' zero of
        LT → Display $ "Please collect your change: " <> show change
          where
          change = coins - remainingCoins
        EQ → Display "Thank you and good bye!"
        GT → Display $ "Please put " <> show remainingCoins' <> " coins in."
    nextState =
      case compare remainingCoins' zero of
        LT → WaitingForTicket
        EQ → WaitingForTicket
        GT → WaitingForCoins remainingCoins'
    nextMachine currentState =
      case currentState of
        WaitingForTicket → parkingMachineAwaitsTicket
        WaitingForCoins cs → parkingMachineAwaitsCoins cs

--------------------------------------------------------------------------------
-- Example 2 -------------------------------------------------------------------

showSink ∷ ∀ s. Show s ⇒ Sink Effect s
showSink = machine \s → do
  Console.log $ "< " <> show s
  pure $ emit_ unit showSink

source ∷ Source Effect Int
source = machine \_ → do
  i ← randomInt 0 100
  Console.log $ "> " <> show i
  pure $ emit_ i source

main ∷ Effect Unit
-- main = runMachine $ fromFoldable [ 1, 2, 3, 4, 5 ] >>> showSink
-- main = runMachine $ source >>> showSink
main = runMachine $ loop (fromFoldable [ 1, 2, 3, 4, 5 ]) >>> showSink
