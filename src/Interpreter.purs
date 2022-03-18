module Interpreter where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Control.Monad.State.Trans (StateT, execStateT, get, modify_)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (logShow)

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

interpret ∷ Asm → AsmM Unit
interpret = case _ of
  Set i r next →
    modify_ (Map.insert r i) *> interpret next
  Mov r1 r2 next → do
    v1 ← lookup r1
    modify_ (Map.insert r2 v1)
    lookup r2 >>= modify_ <<< Map.insert r1
    interpret next
  Add next → do
    st ← get
    -- Homework: rewrite using lookup
    for_ (add <$> Map.lookup A st <*> Map.lookup B st) \c →
      modify_ $ Map.insert C c
    interpret next
  Mul next → do
    st ← get
    -- Homework: rewrite using lookup
    for_ (mul <$> Map.lookup A st <*> Map.lookup B st) \c →
      modify_ $ Map.insert C c
    interpret next
  Ret →
    pure unit

  where
  lookup ∷ Reg → AsmM Int
  lookup _reg = pure 42 -- homework

{- Homework: 

1. Implement `lookup` and use it instead of `for_`
2. Implement `runAsm`/`interpret` using `AsmM2`
2. Implement `runAsm`/`interpret` using `AsmM3`.
4. Write unit tests to prove that implemented functions work correctly!
   
-}

type AsmM2 = ExceptT Error (State St) -- TODO: Homework
type AsmM3 = Effect -- How about state management? 
-- `Effect.Ref` for the rescue!
-- https://pursuit.purescript.org/packages/purescript-refs/5.0.0
