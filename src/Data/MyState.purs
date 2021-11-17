module Data.MyStateT where

import Data.Identity
import Data.Newtype
import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

newtype MyStateT s m a
  = MyStateT (s -> m { nextState :: s, focus :: a })

type MyState s a = MyStateT s Identity a

derive instance newtypeMyStateT :: Newtype (MyStateT s m a) _

instance functorMyState :: Functor m => Functor (MyStateT s m) where
  map ab (MyStateT sas) =
    MyStateT (map (map (\r -> r { focus = ab r.focus })) sas)

instance applyMyState :: Monad m => Apply (MyStateT s m) where
  apply = ap

instance applicativeMyState :: Monad m => Applicative (MyStateT s m) where
  pure a = MyStateT \s -> pure { nextState: s, focus: a }

instance bindMyStateT :: Monad m => Bind (MyStateT s m) where
  bind (MyStateT ma) amb = wrap \s -> do -- m
    { nextState: s', focus: a } <- ma s
    un MyStateT (amb a) s'

instance monadMyStateT :: Monad m => Monad (MyStateT s m)

-- Run a computation in the `MyStateT` monad.
runMyStateT :: ∀ s m a. MyStateT s m a -> s -> m { nextState :: s, focus :: a }
runMyStateT f s = unwrap f s

-- | Run a computation in the `MyStateT` monad, discarding the final state.
evalMyStateT :: ∀ s m a. Functor m => MyStateT s m a -> s -> m a
evalMyStateT f s = runMyStateT f s <#> _.focus

-- -- -- | Run a computation in the `MyStateT` monad discarding the result.
execMyStateT :: ∀ s m a. Functor m => MyStateT s m a -> s -> m s
execMyStateT f s = runMyStateT f s <#> _.nextState

-- get :: forall m s. MonadState s m => m s
get :: ∀ s m. Applicative m => MyStateT s m s
get = MyStateT \s -> pure { nextState: s, focus: s }

-- put :: forall m s. MonadState s m => s -> m Unit
put :: ∀ s m. Applicative m => s -> MyStateT s m Unit
put x = MyStateT \_ -> pure { nextState: x, focus: unit }

-- modify :: forall s m. MonadState s m => (s -> s) -> m s
modify :: ∀ s m. Monad m => (s -> s) -> MyStateT s m Unit
modify f = get >>= f >>> put

testProgram :: ∀ m. Monad m => MyStateT String m Int
testProgram = do
  put "begin"
  let x = 1000
  s <- get
  put $ s <> show x
  pure 42

type Stack = Array

push :: ∀ a m. Monad m => a -> MyStateT (Stack a) m Unit
push x = modify (Array.cons x)

pop :: ∀ a m. Monad m => a -> MyStateT (Stack a) m a
pop a = do
  s <- get
  case Array.uncons s of
    Nothing -> pure a
    Just { head, tail } -> do
      put tail
      pure head

type Calc a = forall m. Monad m => MyStateT (Stack Int) m a

calc :: Calc Int
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

  add :: Calc Unit
  add = op \a b -> a + b

  mul :: Calc Unit
  mul = op \a b -> a * b

  op :: (Int -> Int -> Int) -> Calc Unit
  op f = do
    op1 <- pop 0
    op2 <- pop 0
    push (f op1 op2)

