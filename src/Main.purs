module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"


-- 1. Примитивные типы (скалярные): 
-- 0 ~ Void ~ {    } -> 0  
-- 1 ~ Unit ~ { {} } -> 1
-- 2 ~ type Bool = True | False  -> 2
--  This | That | These ~ This | That | These
-- ~ Bool & Bool


{ 0, 1, 2, 3, 4 }, {(+), (*)} --- Algebraic structure

(+), identity
 
(*), identity

a  -> a 


x op identity = x:
-- 1 + 0 = 1
-- 3 * 1 = 3




data Optional x = None | Some x

data Temperature -- 2
  = Hot
  | Cold

data Either a b = Left a | Right b

data Hot = Hot1 -- ~ 1 ~ Unit
data Cold = Cold -- ~ 1 ~ Unit

Either Unit Unit ~~ Temperature



(True, Hot) 
  :: (Bool, Temperature) 

(False, Hot) 
  :: (Bool, Temperature) 

(True, Cold) 
  :: (Bool, Temperature) 

(False, Cold) 
  :: (Bool, Temperature) 
    

~

data Tuple = Tuple Bool Temperature





data Rocks a -- 2
  = Stone a

type Rocks Temperature = Stone Temperature

data Water -- 2
  = Water Temperature


blueWater :: Water
blueWater = Water Hot

petRock :: Rocks =
petRock = unsafeCoerce blueWater


data Block       -- 4
  = Block1 Water -- 2
  |              -- + 
  Block2 Rocks   -- 2


data Level = 0 | 1 | 2 | 3 | ... | 9 -- 10

data Player =  -- 100
  Food Level   -- 10 
  &            -- * 
  Life Level -- 10 



data World = World -- 1
data Fire = RedFire | BlueFire | GreenFire -- 3


data T12 = T12' World | T12'' Temperature  -- (1 + 2) 
    T12' World | T12'' Hot | T12'' Cold

data T23 = T23' Temperature | T23'' Fire   -- (2 + 3) 
  T23' Hot | T23' Cold | T23'' RedFire | T23'' BlueFire | T23'' Green

data T12_3 = T12_3' T12 | T12_3'' Fire     -- (1 + 2) + 3 
data T1_23 = T1_23' World | T1_23'' T23    -- 1 + (2 + 3)



data Player' = L0F0 | L1F0 | .. L1F9 | L2F0 ... -- 100



data Letter = A | B | C | D 

let l :: Letter = A 


case l of 
  A -> ...
  B -> ...
  C -> ... 
  D -> error?

