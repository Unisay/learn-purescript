module Control.Coroutine.Test where

import Custom.Prelude

import Control.Coroutine (Consumer, Producer, Transducer, await, awaitT, liftStateless, runProducerConsumer, yield, yieldT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), forever, tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)

--------------------------------------------------------------------------------
-- Producer/Consumer test ------------------------------------------------------

producer ∷ Producer Int Effect Unit
producer = do
  let fstNum = 11
  let sndNum = 42
  log $ "Producer: sending first number (" <> show fstNum <> ")..."
  yield fstNum
  log $ "Producer: sending second number (" <> show sndNum <> ")..."
  yield sndNum

consumer ∷ Consumer Int Effect Int
consumer = do
  a ← log "Consumer: waiting for the first number..." *> await
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> await
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

testRunProducerConsumer ∷ Effect Unit
testRunProducerConsumer = do
  _ /\ result ← runProducerConsumer producer consumer
  log $ "Sum is: " <> show result

{-
    > testRunProducerConsumer 
    Producer: sending first number (11)...
    Consumer: waiting for the first number...
    Producer: sending second number (42)...
    Consumer: received first number: 11
    Consumer: waiting for the second number...
    Consumer: received second number: 42
    Sum is: 53
-}

testPrematureProducer ∷ Effect Unit
testPrematureProducer = do
  let badProducer = pass
  void $ runProducerConsumer badProducer consumer

-------- ^ Runtime Error: The producer ended too soon.

consumer2 ∷ Consumer (Maybe Int) Effect (Maybe Int)
consumer2 = runMaybeT do
  a ← log "Consumer: waiting for the first number..." *> MaybeT await
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> MaybeT await
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

testRunProducerConsumer2 ∷ Effect Unit
testRunProducerConsumer2 = do
  _ /\ result ← runProducerConsumer pass consumer2
  log $ "Sum is: " <> show result

--------------------------------------------------------------------------------
-- Transducer tests ------------------------------------------------------------

double ∷ ∀ a m. Monad m ⇒ Transducer a a m Unit
double = liftStateless \a → [ a, a ]

doubleTrouble ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ Transducer a a m Unit
doubleTrouble = awaitT >>= case _ of
  Nothing → pass
  Just a → do
    log $ "Yielding first copy (" <> show a <> ") ..."
    yieldT a
    log $ "Yielding second copy (" <> show a <> ") ..."
    yieldT a

iter2 ∷ Int → Consumer (Maybe Int) Effect Unit
iter2 s = do
  n ← await <* log "Enter a number:"
  case n of
    Nothing → log $ "Sum is: " <> show s
    Just r → iter2 (s + r)

{-

> runProducer $ toProducer $ fromProducer producer >-> double
Producer: sending first number (11)...
Producer: sending second number (42)...
(Tuple [11,11,42,42] (Tuple unit unit))

> runConsumer [Just 3, Nothing] (toConsumer $ double >-> fromConsumer (iter2 0))
Enter a number:
Enter a number:
Enter a number:
Sum is: 6
(Tuple unit unit)

> run (toTrampoline $ fromProducer (yield 3) >-> double >-> fromConsumer (iter2 0))
Enter a number:
Enter a number:
Enter a number:
Sum is: 6
(Tuple unit (Tuple unit unit))

> run (toTrampoline $ fromProducer (yield 3) >-> double >-> double >-> fromConsumer (iter2 0))
Enter a number:
Enter a number:
Enter a number:
Enter a number:
Enter a number:
Sum is: 12
(Tuple unit (Tuple unit (Tuple unit unit)))

-}

iterate ∷ ∀ a m. MonadRec m ⇒ a → (a → m a) → Producer a m Unit
iterate a f = a # tailRecM \i → do
  yield i
  Loop <$> lift (f i)

main ∷ Effect Unit
main = void $ runProducerConsumer p c
  where
  p = iterate 42 (pure <<< add one)
  c = forever $ await >>= \r → logShow r
