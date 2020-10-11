module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Aff (Milliseconds(..), delay, launchAff_)

main :: Effect Unit
main = program

program :: Effect Unit
program = do
  clear
  wait10
  greet

wait10 :: Effect Unit
wait10 = launchAff_ (delay (Milliseconds 10000.0))

greet :: Effect Unit
greet = log "hello"
