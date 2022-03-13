module Interpreter where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Console (log)

data Program ∷ ∀ k. k → Type
data Program a
  = Get (String → Program a)
  | Subscribe (Program a)
  | Unsubscribe (Program a)
  | Stop

get ∷ ∀ a. (String → Program a) → Program a
get sa = Get sa

subscribe ∷ ∀ a. Program a → Program a
subscribe = Subscribe

unsubscribe ∷ ∀ a. Program a → Program a
unsubscribe = Unsubscribe

stop ∷ ∀ a. Program a
stop = Stop

program ∷ Program Unit
program =
  subscribe $
    get case _ of
      "ok" → unsubscribe stop
      _ → stop

type Log = Array String

runLog ∷ ∀ a. Program a → Log
runLog prog = go prog initialLog
  where
  initialLog ∷ Log
  initialLog = []

  go ∷ Program a → Log → Log
  go p log = case p of
    Get k → go (k "ok") (Array.snoc log "get")
    Subscribe k → go k (Array.snoc log "subscribe")
    Unsubscribe k → go k (Array.snoc log "unsubscribe")
    Stop → Array.snoc log "stop"

runEffect ∷ ∀ a. String → Program a → Effect Unit
runEffect userInput = case _ of
  Get k → log "get" *> runEffect userInput (k userInput)
  Subscribe p → log "subscribe" *> runEffect userInput p
  Unsubscribe p → log "unsubscribe" *> runEffect userInput p
  Stop → log "stop"

{-

Registers: A B C
====================
mov <int> <reg>
mov <reg> <reg>
swp <reg> <reg>
add -> C
mul <reg> <reg> -> C 
ret 
====================

1 + 2 + 3 + 4 + 5

mov 1 A 
mov 2 B
add
mov C A
mov 3 B
add 
mov C A
mov 4 B
add 
mov C A
mov 5 B
add 
ret

type St = { a :: Int, b :: Int, c :: Int }
interpret :: Program Int -> State St Int
runState :: State St Int -> St -> Int
-}
