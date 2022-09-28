module Data.MyStateT where

import Prelude

import Control.Monad.State.Class (class MonadState, get, modify, put)
import Data.Array as Array
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, unwrap, wrap)
import Data.Tuple (Tuple(..))

newtype MyStateT s m a = MyStateT (s → m { nextState ∷ s, focus ∷ a })

type MyState s a = MyStateT s Identity a

derive instance Newtype (MyStateT s m a) _

instance Functor m ⇒ Functor (MyStateT s m) where
  map ab (MyStateT sas) =
    MyStateT (map (map (\r → r { focus = ab r.focus })) sas)

instance Monad m ⇒ Apply (MyStateT s m) where
  apply = ap

instance Monad m ⇒ Applicative (MyStateT s m) where
  pure a = MyStateT \s → pure { nextState: s, focus: a }

instance Monad m ⇒ Bind (MyStateT s m) where
  bind (MyStateT ma) amb = wrap \s → do -- m
    { nextState: s', focus: a } ← ma s
    un MyStateT (amb a) s'

instance Monad m ⇒ Monad (MyStateT s m)

instance Monad m ⇒ MonadState s (MyStateT s m) where
  state ∷ ∀ a. (s → (Tuple a s)) → MyStateT s m a
  state f = MyStateT \s → do
    let Tuple focus nextState = f s
    pure { nextState, focus }

-- Run a computation in the `MyStateT` monad.
runMyStateT ∷ ∀ s m a. MyStateT s m a → s → m { nextState ∷ s, focus ∷ a }
runMyStateT f s = unwrap f s

-- | Run a computation in the `MyStateT` monad, discarding the final state.
evalMyStateT ∷ ∀ s m a. Functor m ⇒ MyStateT s m a → s → m a
evalMyStateT f s = runMyStateT f s <#> _.focus

-- -- -- | Run a computation in the `MyStateT` monad discarding the result.
execMyStateT ∷ ∀ s m a. Functor m ⇒ MyStateT s m a → s → m s
execMyStateT f s = runMyStateT f s <#> _.nextState

testProgram ∷ ∀ m. Monad m ⇒ MyStateT String m Int
testProgram = do
  put "begin"
  let x = 1000
  s ← get
  put $ s <> show x
  pure 42

type Stack = Array

push ∷ ∀ a m. Monad m ⇒ a → MyStateT (Stack a) m Unit
push x = void $ modify (Array.cons x)

pop ∷ ∀ a m. Monad m ⇒ a → MyStateT (Stack a) m a
pop a = do
  s ← get
  case Array.uncons s of
    Nothing → pure a
    Just { head, tail } → do
      put tail
      pure head

type Calc a = ∀ m. Monad m ⇒ MyStateT (Stack Int) m a

calc ∷ Calc Int
calc = do
  -- (100 + 42) + (3 * 5)
  do
    push 100
    push 42
    add
  do
    push 3
    push 5
    mul
  add
  pop 0

  where

  add ∷ Calc Unit
  add = op \a b → a + b

  mul ∷ Calc Unit
  mul = op \a b → a * b

  op ∷ (Int → Int → Int) → Calc Unit
  op f = do
    op1 ← pop 0
    op2 ← pop 0
    void $ push (f op1 op2)

