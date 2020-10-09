module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Main as MMM

main :: Effect Unit
main = do
  log (show MMM.add)
