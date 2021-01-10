module Effects.Runtime where

import Prelude
import Effect.Console (clear, log)
import Effect.Unsafe (unsafePerformEffect)

-- "Foreign" functions
clearScreen :: Unit
clearScreen = unsafePerformEffect clear

printLine :: String -> Unit
printLine = unsafePerformEffect <<< log
