module Effects.Runtime where

import Prelude

clearScreen :: Unit
clearScreen = _clearScreen

foreign import _clearScreen :: Unit

printLine :: String -> Unit
printLine = _printLine

foreign import _printLine :: String -> Unit

askForInput :: String -> String
askForInput = _askForInput

foreign import _askForInput :: String -> String
