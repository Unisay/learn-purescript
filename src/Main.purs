module Main where

import Prelude

import Dialog (Dialog, ask, runDialog, say)
import Effect (Effect)
import Effects.Eff (test)

main ∷ Effect Unit
main = test

-- main :: Effect Unit
-- main = runDialog dialog

dialog ∷ Dialog Unit
dialog = do
  say "Welcome to TBOT!"
  name ← ask "What is your name? "
  say "Ok."
  say $ "See you soon, " <> name
  dialog
