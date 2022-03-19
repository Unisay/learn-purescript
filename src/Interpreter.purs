module Interpreter where

import Data.Newtype
import Data.Tuple.Nested
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.State.Trans (StateT, execStateT, modify_)
import Data.Either (Either(..), either)
import Data.Either.Nested ((\/))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref

data Reg = A | B | C

derive instance Eq Reg
derive instance Ord Reg
derive instance Generic Reg _
instance Show Reg where
  show = genericShow

data Asm
  = Set Int Reg Asm
  | Mov Reg Reg Asm
  | Mul Asm
  | Add Asm
  | Ret

type Log = Array String

data Error = EmptyRegister Reg

instance Show Error where
  show = case _ of
    EmptyRegister r → "Register " <> show r <> " is empty."

type St = Map Reg Int
type AsmM = StateT St (Either Error)

-- | Evaluate Asm program as Effect
-- |
-- | > runAsm $ Add $ Set 2 B $ Set 1 A Ret
-- | (fromFoldable [(Tuple A 1),(Tuple B 2)])
-- |
runAsm ∷ Asm → Effect Unit
runAsm =
  interpret
    >>> flip execStateT Map.empty
    >>> either logShow logShow

interpret ∷ ∀ m. MonadThrow Error m ⇒ MonadState St m ⇒ Asm → m Unit
interpret = case _ of
  Set i r next →
    modify_ (Map.insert r i) *> interpret next
  Mov r1 r2 next → do
    v1 ← lookup r1
    modify_ (Map.insert r2 v1)
    lookup r2 >>= modify_ <<< Map.insert r1
    interpret next
  Add next → do
    Tuple a b ← Tuple <$> lookup A <*> lookup B
    modify_ $ Map.insert C $ a + b
    interpret next
  Mul next → do
    Tuple a b ← Tuple <$> lookup A <*> lookup B
    modify_ $ Map.insert C $ a * b
    interpret next
  Ret →
    pure unit
  where
  lookup ∷ Reg → m Int
  lookup reg =
    gets (Map.lookup reg) >>= maybe (throwError $ EmptyRegister reg) pure

{- Homework:
2. Implement `runAsm`/`interpret` using `AsmM3`.
4. Write unit tests to prove that implemented functions work correctly!
-}

type AsmM2 a = ExceptT Error (State St) a

runAsm2 ∷ Asm → Effect Unit
runAsm2 = interpret >>> exec
  where
  exec ∷ AsmM2 Unit → Effect Unit
  exec asm2 = case runState (runExceptT asm2) Map.empty of
    Tuple (Left err) s → Console.log $ show err <> "\n " <> show s
    Tuple (Right _unit) s → Console.logShow s

newtype AsmM3 a = AsmM3 (Effect (Ref (Maybe Error /\ St /\ a)))

derive instance Newtype (AsmM3 a) _

instance Functor AsmM3 where
  map f asm = wrap do
    ra ← unwrap asm
    Tuple error (Tuple st a) ← Ref.read ra
    Ref.new (Tuple error (Tuple st (f a)))

instance Apply AsmM3 where
  apply = ?todo

instance Applicative AsmM3 where
  pure = wrap <<< Ref.new <<< Tuple Nothing <<< Tuple Map.empty

instance Bind AsmM3 where
  bind ∷ ∀ a b. AsmM3 a → (a → AsmM3 b) → AsmM3 b
  bind = ?homework

instance Monad AsmM3

instance MonadState St AsmM3 where
  state ∷ ∀ a. (St → Tuple a St) → AsmM3
  state f = ?homeworkState

instance MonadThrow Error AsmM3 where
  throwError ∷ ∀ a. Error → AsmM3 a
  throwError = ?throwError

runAsm3 ∷ Asm → Effect Unit
runAsm3 = interpret >>> exec
  where
  exec ∷ AsmM3 → Effect Unit
  exec = ?exec
