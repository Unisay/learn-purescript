module Control.Coroutine where

import Custom.Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Functor.Compose (Compose)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)

-- https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

newtype Coroutine f m r = Coroutine (Unit → m (Either (f (Coroutine f m r)) r))

resume ∷ ∀ f m r. Coroutine f m r → m (Either (f (Coroutine f m r)) r)
resume (Coroutine um) = um unit

instance (Functor f, Functor m) ⇒ Functor (Coroutine f m) where
  map f c = Coroutine \_ → bimap (map (map f)) f <$> resume c

instance (Functor f, Monad m) ⇒ Apply (Coroutine f m) where
  apply = ap

instance (Functor f, Monad m) ⇒ Applicative (Coroutine f m) where
  pure = Coroutine <<< const <<< pure <<< Right

instance (Functor f, Monad m) ⇒ Bind (Coroutine f m) where
  bind coroutine f = Coroutine \_ → do
    resume coroutine >>= case _ of
      Left ft → pure $ Left $ bindFlipped f <$> ft
      Right r → resume $ f r

instance (Functor f, Monad m) ⇒ Monad (Coroutine f m)

instance MonadTrans (Coroutine f) where
  lift = Coroutine <<< const <<< map Right

instance (Functor f, MonadEffect m) ⇒ MonadEffect (Coroutine f m) where
  liftEffect = lift <<< liftEffect

suspend ∷ ∀ f m a. Monad m ⇒ f (Coroutine f m a) → Coroutine f m a
suspend = Coroutine <<< const <<< pure <<< Left

type Trampoline m x = Coroutine Identity m x
type Producer a m x = Coroutine (Tuple a) m x
type Consumer a m x = Coroutine (Function a) m x

pause ∷ ∀ m. Monad m ⇒ Trampoline m Unit
pause = suspend (pure pass)

yield ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ x → Producer x m Unit
yield x = suspend (x /\ pass)

await ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ Consumer x m x
await = suspend pure

run ∷ ∀ m x. Monad m ⇒ Trampoline m x → m x
run t = resume t >>= either (run <<< unwrap) pure

runProducer ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → m (Array a /\ x)
runProducer = identity # tailRecM2 \f g →
  resume g <#> case _ of
    Left (a /\ cont) → Loop { a: f <<< Array.cons a, b: cont }
    Right x → Done $ f [] /\ x

runConsumer ∷ ∀ a m x. MonadRec m ⇒ Array a → Consumer a m x → m x
runConsumer = tailRecM2 \is it →
  resume it <#> case Array.uncons is of
    Nothing →
      case _ of
        Left k → Loop { a: [], b: k (unsafeThrow "No more inputs") }
        Right x → Done x
    Just { head, tail } →
      case _ of
        Left k → Loop { a: tail, b: k head }
        Right x → Done x

--------------------------------------------------------------------------------
-- Suspension functors ---------------------------------------------------------

-- | A suspension functor that makes a coroutine which supplies a request
-- | and requires a response before it can proceed.
type RequestResponse request resp = Compose (Tuple request) (Function resp)

-- | A suspension functor that makes a coroutine which can either demand
-- | or supply a value every time it suspends, but not both at the same time.
-- type DemandSupply demand supply = Coproduct (Function demand) (Tuple supply)
data DemandSupply demand supply k
  = Demand (demand → k)
  | Supply (supply /\ k)

derive instance Functor (DemandSupply d s)

--------------------------------------------------------------------------------
-- Transducer ------------------------------------------------------------------

type Transducer a b m x = Coroutine (DemandSupply (Maybe a) b) m x

yieldT ∷ ∀ m a b. Monad m ⇒ b → Transducer a b m Unit
yieldT b = suspend (Supply (b /\ pass))

awaitT ∷ ∀ m a b. Monad m ⇒ Transducer a b m (Maybe a)
awaitT = suspend (Demand pure)

liftT ∷ ∀ m a b. Monad m ⇒ (a → b) → Transducer a b m Unit
liftT f = awaitT >>= maybe pass \a → yieldT (f a) *> liftT f

liftStateless ∷ ∀ m a b. Monad m ⇒ (a → Array b) → Transducer a b m Unit
liftStateless f =
  awaitT >>= maybe pass \a → traverse yieldT (f a) *> liftStateless f

liftStateful
  ∷ ∀ m a b s
  . Monad m
  ⇒ (s → a → s /\ Array b)
  → (s → Array b)
  → s
  → Transducer a b m Unit
liftStateful step eof state = awaitT >>= case _ of
  Nothing → for_ (eof state) yieldT
  Just a → do
    let nextState /\ bs = step state a
    for_ bs yieldT *> liftStateful step eof nextState

composeTransducers
  ∷ ∀ a b c m x y
  . Monad m
  ⇒ Transducer a b m x
  → Transducer b c m y
  → Transducer a c m (x /\ y)
composeTransducers t1 t2 = Coroutine \_ → do
  e1 ← resume t1
  e2 ← resume t2
  case e1, e2 of
    Right x, Right y →
      pure $ Right $ x /\ y
    Left (Demand f), e →
      pure $ Left $ Demand \a → f a >-> Coroutine \_ → pure e
    e, Left (Supply t) →
      pure $ Left $ Supply $ composeTransducers (Coroutine \_ → pure e) <$> t
    Left (Supply (b /\ k)), Left (Demand f) →
      resume $ k >-> f (Just b)
    Left (Supply (_ /\ k)), Right y →
      resume $ k >-> pure y
    Right x, Left (Demand f) →
      resume $ pure x >-> f Nothing

infixr 9 composeTransducers as >->

hoistCoroutine
  ∷ ∀ s w m x
  . Functor s
  ⇒ Monad m
  ⇒ s ~> w
  → Coroutine s m x
  → Coroutine w m x
hoistCoroutine nt c = Coroutine \_ →
  resume c <#> lmap (map (hoistCoroutine nt) >>> nt)

fromProducer ∷ ∀ a m x. Monad m ⇒ Producer a m x → Transducer Void a m x
fromProducer = hoistCoroutine Supply

fromConsumer ∷ ∀ a m x. Monad m ⇒ Consumer (Maybe a) m x → Transducer a Void m x
fromConsumer = hoistCoroutine Demand

toProducer ∷ ∀ a m x. Monad m ⇒ Transducer Void a m x → Producer a m x
toProducer = hoistCoroutine case _ of
  Demand _impossible → unsafeThrow "Transducer.toProducer: Demand"
  Supply t → t

toConsumer ∷ ∀ a m x. Monad m ⇒ Transducer a Void m x → Consumer (Maybe a) m x
toConsumer = hoistCoroutine case _ of
  Demand f → f
  Supply _impossible → unsafeThrow "Transducer.toConsumer: Supply"

toTrampoline ∷ ∀ m x. Monad m ⇒ Transducer Void Void m x → Trampoline m x
toTrampoline = hoistCoroutine case _ of
  Demand _impossible → unsafeThrow "Transducer.toTrampoline: Demand"
  Supply (_ /\ k) → Identity k

--------------------------------------------------------------------------------
-- Producer/Consumer test ------------------------------------------------------

-- | A helper function that really belongs in Control.Monad
bindM2 ∷ ∀ m a b c. Monad m ⇒ (a → b → m c) → m a → m b → m c
bindM2 f ma mb = ma >>= \a → mb >>= f a

pipe1 ∷ ∀ m a x y. Monad m ⇒ Producer a m x → Consumer a m y → m (x /\ y)
pipe1 g i = bindM2 proceed (resume g) (resume i)
  where
  proceed = case _, _ of
    Left (a /\ k), Left f → pipe1 k (f a)
    Left (_ /\ k), Right y → pipe1 k (pure y)
    Right _, Left _ → unsafeThrow "The producer ended too soon."
    Right x, Right y → pure (x /\ y)

pipe2
  ∷ ∀ m a x y. Monad m ⇒ Producer a m x → Consumer (Maybe a) m y → m (x /\ y)
pipe2 g i = bindM2 proceed (resume g) (resume i)
  where
  proceed = case _, _ of
    Left (a /\ k), Left f → pipe2 k (f $ Just a)
    Left (_ /\ k), Right y → pipe2 k (pure y)
    Right x, Left f → pipe2 (pure x) (f Nothing)
    Right x, Right y → pure (x /\ y)

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

testPipe1 ∷ Effect Unit
testPipe1 = do
  _ /\ result ← pipe1 producer consumer
  log $ "Sum is: " <> show result

{-
    > testPipe1 
    Producer: sending first number (11)...
    Consumer: waiting for the first number...
    Producer: sending second number (42)...
    Consumer: received first number: 11
    Consumer: waiting for the second number...
    Consumer: received second number: 42
    Sum is: 53
-}

testPipe1TooSoon ∷ Effect Unit
testPipe1TooSoon = do
  let badProducer = pass
  void $ pipe1 badProducer consumer

-------- ^ Runtime Error: The producer ended too soon.

iteratee2 ∷ Consumer (Maybe Int) Effect (Maybe Int)
iteratee2 = runMaybeT do
  a ← log "Consumer: waiting for the first number..." *> MaybeT await
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> MaybeT await
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

testPipe2 ∷ Effect Unit
testPipe2 = do
  _ /\ result ← pipe2 pass iteratee2
  log $ "Sum is: " <> show result

double ∷ ∀ a m. Monad m ⇒ Transducer a a m Unit
double = liftStateless \a → [ a, a ]

doubleTrouble ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ Transducer a a m Unit
doubleTrouble = do
  awaitT >>= case _ of
    Nothing → pass
    Just a → do
      log $ "Yielding first copy (" <> show a <> ") ..."
      yieldT a
      log $ "Yielding second copy (" <> show a <> ") ..."
      yieldT a

{-

> runProducer $ toProducer $ fromProducer producer >-> double
Yielding one, then two, returning three: ([1,1,2,2],(3,()))

> runConsumer [Just 3, Nothing] (toConsumer $ double >-> fromConsumer (iter2 0))
Enter a number: Enter a number: Enter a number: sum is 6
((),())

> run (toTrampoline $ fromProducer (yield 3) >-> double >-> fromConsumer (iter2 0))
Enter a number: Enter a number: Enter a number: sum is 6
(((),()),())

> run (toTrampoline $ fromProducer (yield 3) >-> double >-> double >-> fromConsumer (iter2 0))
Enter a number: Enter a number: Enter a number: Enter a number:
Enter a number: sum is 12
((((),()),()),())
-}
