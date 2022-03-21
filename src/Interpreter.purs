module Interpreter where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.State.Trans (StateT, execStateT, modify_)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
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

newtype AsmM3 a = AsmM3 (Effect (Ref (ExceptT Error (Tuple St) a)))

derive instance Newtype (AsmM3 a) _

instance Functor AsmM3 where
  map f asm = wrap do
    ea ← Ref.read =<< unwrap asm
    Ref.new $ f <$> ea

instance Apply AsmM3 where
  apply ∷ ∀ a b. AsmM3 (a → b) → AsmM3 a → AsmM3 b
  apply = ap

instance Applicative AsmM3 where
  pure ∷ ∀ a. a → AsmM3 a
  pure = wrap <<< Ref.new <<< wrap <<< Tuple Map.empty <<< pure

instance Bind AsmM3 where
  bind ∷ ∀ a b. AsmM3 a → (a → AsmM3 b) → AsmM3 b
  bind asm f = wrap do
    unwrap asm >>= Ref.read >>= runExceptT >>> case _ of
      Tuple sta (Left err) → Ref.new $ wrap $ Tuple sta $ Left err
      Tuple sta (Right a) → do
        unwrap (f a) >>= Ref.read >>= runExceptT >>> case _ of
          Tuple stb (Left err) →
            let
              st' = Map.unionWith (flip const) sta stb
            in
              Ref.new $ wrap $ Tuple st' $ Left err
          Tuple stb (Right b) →
            let
              st' = Map.unionWith (flip const) sta stb
            in
              Ref.new $ wrap $ Tuple st' $ Right b

instance Monad AsmM3

instance MonadThrow Error AsmM3 where
  throwError ∷ ∀ a. Error → AsmM3 a
  throwError = wrap <<< Ref.new <<< wrap <<< Tuple Map.empty <<< Left

runAsm3 ∷ Asm → Effect Unit
runAsm3 asm = do
  r ← Ref.new $ Tuple (Map.empty ∷ Map Reg Int) (Nothing ∷ Maybe Error)
  go r asm
  where
  go r = case _ of
    Set i reg next → do
      Tuple st mbError ← Ref.read r
      case mbError of
        Just err →
          showErr err st
        Nothing → do
          Ref.write (Tuple (Map.insert reg i st) Nothing) r
          go r next
    Mov r1 r2 next → do
      Tuple st mbError ← Ref.read r
      case mbError of
        Just err → showErr err st
        Nothing → do
          case Map.lookup r1 st, Map.lookup r2 st of
            Nothing, _ → showErr (EmptyRegister r1) st
            _, Nothing → showErr (EmptyRegister r2) st
            Just v1, Just v2 →
              let
                st' = Map.insert r1 v2 <<< Map.insert r2 v1 $ st
              in
                Ref.write (Tuple st' Nothing) r *> go r next
    Add next → do
      Tuple st mbError ← Ref.read r
      case mbError of
        Just err → showErr err st
        Nothing → do
          case Map.lookup A st, Map.lookup B st of
            Nothing, _ → showErr (EmptyRegister A) st 
            _, Nothing → showErr (EmptyRegister B) st 
            Just a, Just b → 
              let st' = Map.insert C (a + b) st
              in Ref.write (Tuple st' Nothing) r *> go r next
    Mul next → do
      Tuple st mbError ← Ref.read r
      case mbError of
        Just err → showErr err st
        Nothing → do
          case Map.lookup A st, Map.lookup B st of
            Nothing, _ → showErr (EmptyRegister A) st 
            _, Nothing → showErr (EmptyRegister B) st 
            Just a, Just b → 
              let st' = Map.insert C (a * b) st
              in Ref.write (Tuple st' Nothing) r *> go r next
    Ret →
      pure unit
  
  showErr :: Error -> St -> Effect Unit 
  showErr err st = Console.log $ show err <> "\n " <> show st 