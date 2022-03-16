module Machine where

import Prelude hiding (add, mul)

import Applicative (lift2)
import Control.Monad.State (gets, modify_)
import Control.Monad.State.Trans (StateT, execStateT)
import Control.Monad.Writer (Writer)
import Control.Monad.Writer.Class (tell)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.MyFree (Free, foldFree, liftF, showFree)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Motsunabe (Doc(..))

data Reg = A | B | C

instance Show Reg where
  show = case _ of
    A → "A"
    B → "B"
    C → "C"

derive instance Eq Reg
derive instance Ord Reg

data AsmF ∷ Type → Type
data AsmF a
  = Set Int Reg a
  | Mov Reg Reg a
  | Swp Reg Reg a
  | Rnd (Int → a)
  | Add a
  | Mul a

derive instance Functor AsmF

instance Show a ⇒ Show (AsmF a) where
  show = case _ of
    Set i r _ → "set " <> show i <> " " <> show r
    Mov a b _ → "mov " <> show a <> " " <> show b
    Swp a b _ → "swp " <> show a <> " " <> show b
    Rnd _ → "rnd"
    Add _ → "add"
    Mul _ → "mul"

type Asm a = Free AsmF a

set ∷ Int → Reg → Asm Unit
set i r = liftF $ Set i r unit

mov ∷ Reg → Reg → Asm Unit
mov a b = liftF $ Mov a b unit

swp ∷ Reg → Reg → Asm Unit
swp a b = liftF $ Swp a b unit

rnd ∷ Asm Int
rnd = liftF $ Rnd identity

add ∷ Asm Unit
add = liftF $ Add unit

mul ∷ Asm Unit
mul = liftF $ Mul unit

-- | 1 + 2 + 3 + 4 + 5 + rnd * rnd
program ∷ Asm Unit
program = do
  set 1 A
  set 2 B
  add
  mov C A
  set 3 B
  add
  mov C A
  set 4 B
  add
  mov C A
  set 5 B
  add
  r ← rnd
  set r A
  t ← rnd
  set t B
  mul

type Registers = Map Reg (Maybe Int)

runAsm ∷ Asm Unit → Effect (Maybe Int)
runAsm asm =
  execStateT (foldFree interpret asm) Map.empty
    <#> Map.lookup C >>> join
  where
  interpret ∷ AsmF ~> StateT Registers Effect
  interpret = case _ of
    Set i r x →
      setR r (Just i) $> x
    Mov a b x →
      (getR a >>= setR b) $> x
    Rnd k →
      liftEffect $ k <$> randomInt 0 100
    Swp a b x →
      x <$ do
        ai ← getR a
        bi ← getR b
        setR a bi
        setR b ai
    Add x →
      x <$ do
        a ← getR A
        b ← getR B
        traverse_ (setR C <<< Just) (lift2 (+) a b)
    Mul x →
      x <$ do
        a ← getR A
        b ← getR B
        traverse_ (setR C <<< Just) (lift2 (*) a b)

  getR ∷ Reg → StateT Registers Effect (Maybe Int)
  getR r = gets $ Map.lookup r >>> join

  setR ∷ Reg → Maybe Int → StateT Registers Effect Unit
  setR r i = modify_ (Map.alter (\_mv → Just i) r)

showProgram ∷ ∀ a. Asm a → String
showProgram = showFree f
  where
  f ∷ ∀ x. AsmF x → Writer Doc x
  f = case _ of
    Set i r x → t x $ "set " <> show i <> " " <> show r
    Mov a b x → t x $ "mov " <> show a <> " " <> show b
    Swp a b x → t x $ "swp " <> show a <> " " <> show b
    Rnd k → t (k 42) $ "rnd"
    Add x → t x "add"
    Mul x → t x "mul"
    where
    t ∷ x → String → Writer Doc x
    t x m = tell (DText m <> DLine) $> x
