module App.Web.Dialog.Foreign where

import Effect (Effect)
import Data.Unit (Unit)

foreign import ask :: String -> Effect String

foreign import tell :: String -> Effect Unit
