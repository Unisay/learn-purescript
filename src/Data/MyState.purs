module Data.MyState where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

newtype MyState s a
  = MyState (s -> { nextState :: s, focus :: a })

instance functorMyState :: Functor (MyState s) where
  map :: forall a b. (a -> b) -> MyState s a -> MyState s b
  map ab (MyState sas) = MyState (map (\r -> r { focus = ab r.focus }) sas)

instance applyMyState :: Apply (MyState s) where
  apply (MyState sab) (MyState sa) = MyState \s ->
    let
      { nextState: s', focus: ab } = sab s
      { nextState: s'', focus: a } = sa s'
    in
      { nextState: s'', focus: ab a }

instance applicativeMyState :: Applicative (MyState s) where
  pure a = MyState \s -> { nextState: s, focus: a }

instance bindMyState :: Bind (MyState s) where
  bind (MyState ma) amb = MyState \s ->
    let
      { nextState: s', focus: a } = ma s
      MyState sb = amb a
    in
      sb s'

-- | Run a computation in the `MyState` monad.
runMyState :: ∀ s a. MyState s a -> s -> { nextState :: s, focus :: a }
runMyState (MyState f) s = f s

-- -- | Run a computation in the `MyState` monad, discarding the final state.
evalMyState :: ∀ s a. MyState s a -> s -> a
evalMyState (MyState f) s = (f s).focus

-- -- | Run a computation in the `MyState` monad discarding the result.
execMyState :: ∀ s a. MyState s a -> s -> s
execMyState (MyState f) s = (f s).nextState

get :: forall s. MyState s s
get = MyState \s -> { nextState: s, focus: s }

put :: forall x. x -> MyState x Unit
put x = MyState \_ -> { nextState: x, focus: unit }

modify :: forall s. (s -> s) -> MyState s Unit
modify f = do
  s <- get
  put (f s)

testProgram :: MyState String Int
testProgram = do
  put "begin"
  let x = 1000
  s <- get
  put $ s <> show x
  pure 42

type Stack = Array

push :: forall a. a -> MyState (Stack a) Unit
push x = modify (Array.cons x)

pop :: forall a. a -> MyState (Stack a) a
pop a = do
  s <- get
  case Array.uncons s of
    Nothing -> pure a
    Just { head, tail } -> do
      put tail
      pure head

type Calc = MyState (Stack Int)

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

