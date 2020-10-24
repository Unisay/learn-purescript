module Main where

import Prelude
import Dialog (ask, cls, runDialog, say)
import Effect (Effect)

main :: Effect Unit
main =
  runDialog do
    cls
    say "Welcome to TBOT!"
    name <- ask "What is your name? "
    say "Ok."
    say ("See you soon, " <> name)
