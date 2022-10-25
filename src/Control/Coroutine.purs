module Control.Coroutine where

import Control.Bind (bindFlipped)
import Control.Coroutine.Suspension.Functor (Consume(..), DemandSupply(..), Join, Produce, Split, fst, produce, snd)
import Control.Monad.Rec.Class (class MonadRec, Step(..), loop2, tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Custom.Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), Either(..), Maybe(..), Unit, Void, ap, bind, const, either, identity, map, maybe, pass, pure, unit, (#), ($), (*>), (<#>), (<$>), (<<<), (>>=), (>>>))
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Functor.Coproduct (left, right)
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
type Producer a m x = Coroutine (Produce a) m x
type Consumer a m x = Coroutine (Consume a) m x

pause ∷ ∀ m. Monad m ⇒ Trampoline m Unit
pause = suspend (pure pass)

yield ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ x → Producer x m Unit
yield x = suspend (produce x pass)

await ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ Consumer x m x
await = suspend (Consume pure)

run ∷ ∀ m x. Monad m ⇒ Trampoline m x → m x
run t = resume t >>= either (run <<< unwrap) pure

runProducer ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → m (Produce (Array a) x)
runProducer = identity # tailRecM2 \f g →
  resume g <#> case _ of
    Left p → loop2 (f <<< Array.cons (fst p)) (snd p)
    Right x → Done $ produce (f []) x

runConsumer ∷ ∀ a m x. MonadRec m ⇒ Array a → Consumer a m x → m x
runConsumer = tailRecM2 \is it →
  resume it <#> case Array.uncons is of
    Nothing → case _ of
      Left (Consume k) → loop2 [] (k (unsafeThrow "No more inputs"))
      Right x → Done x
    Just { head, tail } → case _ of
      Left (Consume k) → loop2 tail (k head)
      Right x → Done x

runProducerConsumer
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer a m y → m (x /\ y)
runProducerConsumer = tailRecM2 \producer consumer → do
  l ← resume producer
  r ← resume consumer
  pure case l, r of
    Left p, Left (Consume f) → loop2 (snd p) (f (fst p))
    Left p, Right y → loop2 (snd p) (pure y)
    Right _, Left _ → unsafeThrow "The producer ended too soon."
    Right x, Right y → Done $ x /\ y

runProducerConsumer'
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer (Maybe a) m y → m (x /\ y)
runProducerConsumer' = tailRecM2 \producer consumer → do
  l ← resume producer
  r ← resume consumer
  pure case l, r of
    Left p, Left (Consume f) → loop2 (snd p) (f (Just (fst p)))
    Left p, Right y → loop2 (snd p) (pure y)
    Right x, Left (Consume f) → loop2 (pure x) (f Nothing)
    Right x, Right y → Done (x /\ y)

--------------------------------------------------------------------------------
-- Transducer ------------------------------------------------------------------

type Transducer a b m x = Coroutine (DemandSupply (Maybe a) b) m x

yieldT ∷ ∀ m a b. Monad m ⇒ b → Transducer a b m Unit
yieldT b = suspend (Supply (produce b pass))

awaitT ∷ ∀ m a b. Monad m ⇒ Transducer a b m (Maybe a)
awaitT = suspend (Demand (Consume pure))

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
    Left (Demand (Consume f)), e →
      pure $ Left $ Demand $ Consume \a → f a >-> Coroutine \_ → pure e
    e, Left (Supply t) →
      pure $ Left $ Supply $ composeTransducers (Coroutine \_ → pure e) <$> t
    Left (Supply p), Left (Demand (Consume f)) →
      resume $ snd p >-> f (Just (fst p))
    Left (Supply p), Right y →
      resume $ snd p >-> pure y
    Right x, Left (Demand (Consume f)) →
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
  Supply p → Identity (snd p)

--------------------------------------------------------------------------------
-- Branching
--------------------------------------------------------------------------------

type Splitter a m x = Coroutine (Split a) m x

type Joiner a m x = Coroutine (Join a) m x

yieldLeft ∷ ∀ a m. Monad m ⇒ a → Splitter a m Unit
yieldLeft a = suspend (right (left (produce a pass)))

yieldRight ∷ ∀ a m. Monad m ⇒ a → Splitter a m Unit
yieldRight a = suspend (right (right (produce a pass)))

awaitLeft ∷ ∀ a m. Monad m ⇒ Joiner a m (Maybe a)
awaitLeft = suspend (left (left (Consume pure)))

awaitRight ∷ ∀ a m. Monad m ⇒ Joiner a m (Maybe a)
awaitRight = suspend (left (right (Consume pure)))

ifThenElse
  ∷ ∀ a b m x y z
  . Monad m
  ⇒ Splitter a m x
  → Transducer a b m y
  → Transducer a b m z
  → Transducer a b m (x /\ y /\ z)
ifThenElse splitter tr1 tr2 = ?x

-- not :: Monad m ⇒ Splitter a m x → Splitter a m x
-- and :: Monad m ⇒ Splitter a m x → Splitter a m x → Splitter a m x
-- or :: Monad m ⇒ Splitter a m x → Splitter a m x → Splitter a m x
-- groupBy :: Monad m ⇒ Splitter a m x → Transducer a [a ] m x
-- any :: Monad m ⇒ Splitter a m x → Splitter [a ] m x
-- all :: Monad m ⇒ Splitter a m x → Splitter [a ] m x
