module Data.MyState where

import Prelude
import Control.Alt (class Alt)
import Data.Tuple (Tuple, fst, snd)
import Homework.Todo (todo, todo')

newtype MyState s a
  = MyState (s -> Tuple a s)

instance functorMyState :: Functor (MyState s) where
  map = todo' "Implement"

instance applyMyState :: Apply (MyState s) where
  apply = todo' "Implement"

instance applicativeMyState :: Applicative (MyState s) where
  pure = todo "Implement"

instance bindMyState :: Bind (MyState s) where
  bind = todo' "Implement"

instance altMyState :: Alt (MyState s) where
  alt = todo "Implement"

-- | Run a computation in the `MyState` monad.
runMyState :: ∀ s a. MyState s a -> s -> Tuple a s
runMyState (MyState f) s = f s

-- | Run a computation in the `MyState` monad, discarding the final state.
evalMyState :: ∀ s a. MyState s a -> s -> a
evalMyState (MyState f) s = todo "Implement"

-- | Run a computation in the `MyState` monad discarding the result.
execMyState :: ∀ s a. MyState s a -> s -> s
execMyState (MyState f) s = todo "Implement"

-- | Change the result type in a `MyState` monad action.
mapMyState :: ∀ s a b. (Tuple a s -> Tuple b s) -> MyState s a -> MyState s b
mapMyState k (MyState f) = todo "Implement"

-- | Modify the final state in a `MyState` monad action.
withMyState :: ∀ s a. (s -> s) -> MyState s a -> MyState s a
withMyState k (MyState f) = todo "Implement"
