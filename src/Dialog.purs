module Dialog where

import Prelude

import Control.Monad.Reader as R
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (clear, log)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)

type Dialog a = R.ReaderT Interface Aff a

say ∷ String → Dialog Unit
say msg = R.ReaderT (\_ → log msg)

cls ∷ Dialog Unit
cls = R.ReaderT (\_ → clear)

ask ∷ String → Dialog String
ask msg =
  R.ReaderT
    $ \iface →
        makeAff \cb → question msg (cb <<< Right) iface $> nonCanceler

runDialog ∷ Dialog Unit → Effect Unit
runDialog dialog = launchAff_ do
  bracket initIface closeIface (useIface dialog)

  where
  initIface ∷ Aff Interface
  initIface = liftEffect (createConsoleInterface noCompletion)

  closeIface ∷ Interface → Aff Unit
  closeIface = liftEffect <<< close

  useIface ∷ Dialog Unit → Interface → Aff Unit
  useIface = R.runReaderT
