module Control.Coroutine where

import Custom.Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Rec.Class (class MonadRec, Step(..), loop2, tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Functor.Compose (Compose)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)

-- https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

newtype Coroutine f m r =
  Coroutine
    ( Unit -- This "thunk" protects from stack overflow.
      → m (Either (f (Coroutine f m r)) r)
    )

resume ∷ ∀ f m r. Coroutine f m r → m (Either (f (Coroutine f m r)) r)
resume (Coroutine um) = um unit

instance (Functor f, Functor m) ⇒ Functor (Coroutine f m) where
  map f c = Coroutine \_ → bimap (map (map f)) f <$> resume c

instance (Functor f, Monad m) ⇒ Apply (Coroutine f m) where
  apply = ap

instance (Functor f, Monad m) ⇒ Applicative (Coroutine f m) where
  pure = Coroutine <<< const <<< pure <<< Right

instance (Functor f, Monad m) ⇒ Bind (Coroutine f m) where
  bind coroutine f = Coroutine \_ →
    resume coroutine >>= case _ of
      Left ft → pure $ Left $ bindFlipped f <$> ft
      Right r → resume $ f r

instance (Functor f, Monad m) ⇒ Monad (Coroutine f m)

instance (Monad m, Functor f) ⇒ MonadRec (Coroutine f m) where
  tailRecM f = go
    where
    go s = f s >>= case _ of
      Loop a → go a
      Done b → pure b

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
    Left (a /\ cont) → loop2 (f <<< Array.cons a) cont
    Right x → Done $ f [] /\ x

runConsumer ∷ ∀ a m x. MonadRec m ⇒ Array a → Consumer a m x → m x
runConsumer = tailRecM2 \is it →
  resume it <#> case Array.uncons is of
    Nothing → case _ of
      Left k → loop2 [] (k (unsafeThrow "No more inputs"))
      Right x → Done x
    Just { head, tail } → case _ of
      Left k → loop2 tail (k head)
      Right x → Done x

runProducerConsumer
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer a m y → m (x /\ y)
runProducerConsumer = tailRecM2 \p c → do
  l ← resume p
  r ← resume c
  pure case l, r of
    Left (a /\ k), Left f → loop2 k (f a)
    Left (_ /\ k), Right y → loop2 k (pure y)
    Right _, Left _ → unsafeThrow "The producer ended too soon."
    Right x, Right y → Done $ x /\ y

runProducerConsumer'
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer (Maybe a) m y → m (x /\ y)
runProducerConsumer' = tailRecM2 \p c → do
  l ← resume p
  r ← resume c
  pure case l, r of
    Left (a /\ k), Left f → loop2 k (f (Just a))
    Left (_ /\ k), Right y → loop2 k (pure y)
    Right x, Left f → loop2 (pure x) (f Nothing)
    Right x, Right y → Done (x /\ y)

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
