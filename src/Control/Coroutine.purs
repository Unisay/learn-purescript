module Control.Coroutine where

import Custom.Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Functor.Compose (Compose)
import Data.Functor.Coproduct (Coproduct(..), left, right)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)

-- https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

newtype Coroutine f m r = Coroutine (m (Either (f (Coroutine f m r)) r))

resume ∷ ∀ f m r. Coroutine f m r → m (Either (f (Coroutine f m r)) r)
resume (Coroutine m) = m

instance (Functor f, Functor m) ⇒ Functor (Coroutine f m) where
  map f (Coroutine m) = Coroutine (bimap (map (map f)) f <$> m)

instance (Functor f, Monad m) ⇒ Apply (Coroutine f m) where
  apply = ap

instance (Functor f, Monad m) ⇒ Applicative (Coroutine f m) where
  pure = Coroutine <<< pure <<< Right

instance (Functor f, Monad m) ⇒ Bind (Coroutine f m) where
  bind coroutine f = Coroutine do
    resume coroutine >>= case _ of
      Left ft → pure $ Left $ bindFlipped f <$> ft
      Right r → resume $ f r

instance (Functor f, Monad m) ⇒ Monad (Coroutine f m)

instance MonadTrans (Coroutine f) where
  lift = Coroutine <<< liftA1 Right

instance (Functor f, MonadEffect m) ⇒ MonadEffect (Coroutine f m) where
  liftEffect = lift <<< liftEffect

suspend ∷ ∀ f m a. Monad m ⇒ f (Coroutine f m a) → Coroutine f m a
suspend = Coroutine <<< pure <<< Left

type Trampoline m x = Coroutine Identity m x
type Generator a m x = Coroutine (Tuple a) m x
type Iteratee a m x = Coroutine (Function a) m x

pause ∷ ∀ m. Monad m ⇒ Trampoline m Unit
pause = suspend (pure pass)

yield ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ x → Generator x m Unit
yield x = suspend (x /\ pass)

await ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ Iteratee x m x
await = suspend pure

run ∷ ∀ m x. Monad m ⇒ Trampoline m x → m x
run t = resume t >>= either (run <<< unwrap) pure

runGenerator ∷ ∀ a m x. MonadRec m ⇒ Generator a m x → m (Array a /\ x)
runGenerator = identity # tailRecM2 \f g →
  resume g <#> case _ of
    Left (a /\ cont) → Loop { a: f <<< Array.cons a, b: cont }
    Right x → Done $ f [] /\ x

runIteratee ∷ ∀ a m x. MonadRec m ⇒ Array a → Iteratee a m x → m x
runIteratee = tailRecM2 \is it →
  resume it <#> case Array.uncons is of
    Nothing →
      case _ of
        Left k → Loop { a: [], b: k (unsafeThrow "No more inputs") }
        Right x → Done x
    Just { head, tail } →
      case _ of
        Left k → Loop { a: tail, b: k head }
        Right x → Done x

mapSuspension
  ∷ ∀ s w m x
  . Functor s
  ⇒ Monad m
  ⇒ s ~> w
  → Coroutine s m x
  → Coroutine w m x
mapSuspension nt = Coroutine <<< liftA1 f <<< resume
  where
  f = lmap (nt <<< map (mapSuspension nt))

--------------------------------------------------------------------------------
-- Suspension functors ---------------------------------------------------------

-- | A suspension functor that makes a coroutine which supplies a request
-- | and requires a response before it can proceed.
type RequestResponse request resp = Compose (Tuple request) (Function resp)

-- | A suspension functor that makes a coroutine which can either demand
-- | or supply a value every time it suspends, but not both at the same time.
type DemandSupply demand supply = Coproduct (Function demand) (Tuple supply)

--------------------------------------------------------------------------------
-- Transducer ------------------------------------------------------------------

type Transducer a b m x = Coroutine (DemandSupply (Maybe a) b) m x

yieldT ∷ ∀ m a b. Monad m ⇒ b → Transducer a b m Unit
yieldT b = suspend (right (b /\ pass))

awaitT ∷ ∀ m a b. Monad m ⇒ Transducer a b m (Maybe a)
awaitT = suspend (left pure)

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
composeTransducers t1 t2 = Coroutine do
  e1 ← resume t1
  e2 ← resume t2
  case e1, e2 of
    Right x, Right y →
      pure $ Right $ x /\ y
    Left (Coproduct (Left f)), e →
      pure $ Left $ left \a → f a `composeTransducers` Coroutine (pure e)
    e, Left (Coproduct (Right t)) →
      pure $ Left $ right $ composeTransducers (Coroutine (pure e)) `map` t
    Left (Coproduct (Right (b /\ k))), Left (Coproduct (Left f)) →
      resume $ k `composeTransducers` f (Just b)
    Left (Coproduct (Right (_ /\ k))), Right y →
      resume $ k `composeTransducers` pure y
    Right x, Left (Coproduct (Left f)) →
      resume $ pure x `composeTransducers` f Nothing

--------------------------------------------------------------------------------
-- Producer/Consumer test ------------------------------------------------------

-- | A helper function that really belongs in Control.Monad
bindM2 ∷ ∀ m a b c. Monad m ⇒ (a → b → m c) → m a → m b → m c
bindM2 f ma mb = ma >>= \a → mb >>= f a

pipe1 ∷ ∀ m a x y. Monad m ⇒ Generator a m x → Iteratee a m y → m (x /\ y)
pipe1 g i = bindM2 proceed (resume g) (resume i)
  where
  proceed = case _, _ of
    Left (a /\ k), Left f → pipe1 k (f a)
    Left (_ /\ k), Right y → pipe1 k (pure y)
    Right _, Left _ → unsafeThrow "The producer ended too soon."
    Right x, Right y → pure (x /\ y)

pipe2
  ∷ ∀ m a x y. Monad m ⇒ Generator a m x → Iteratee (Maybe a) m y → m (x /\ y)
pipe2 g i = bindM2 proceed (resume g) (resume i)
  where
  proceed = case _, _ of
    Left (a /\ k), Left f → pipe2 k (f $ Just a)
    Left (_ /\ k), Right y → pipe2 k (pure y)
    Right x, Left f → pipe2 (pure x) (f Nothing)
    Right x, Right y → pure (x /\ y)

generator ∷ Generator Int Effect Unit
generator = do
  let fstNum = 11
  let sndNum = 42
  log $ "Generator: sending first number (" <> show fstNum <> ")..."
  yield fstNum
  log $ "Generator: sending second number (" <> show sndNum <> ")..."
  yield sndNum

iteratee ∷ Iteratee Int Effect Int
iteratee = do
  a ← log "Iteratee: waiting for the first number..." *> await
  log $ "Iteratee: received first number: " <> show a
  b ← log "Iteratee: waiting for the second number..." *> await
  log $ "Iteratee: received second number: " <> show b
  pure $ a + b

testPipe1 ∷ Effect Unit
testPipe1 = do
  _ /\ result ← pipe1 generator iteratee
  log $ "Sum is: " <> show result

{-
    > testPipe1 
    Generator: sending first number (11)...
    Iteratee: waiting for the first number...
    Generator: sending second number (42)...
    Iteratee: received first number: 11
    Iteratee: waiting for the second number...
    Iteratee: received second number: 42
    Sum is: 53
-}

testPipe1TooSoon ∷ Effect Unit
testPipe1TooSoon = do
  let badGenerator = pass
  void $ pipe1 badGenerator iteratee

-------- ^ Runtime Error: The producer ended too soon.

iteratee2 ∷ Iteratee (Maybe Int) Effect (Maybe Int)
iteratee2 = runMaybeT do
  a ← log "Iteratee: waiting for the first number..." *> MaybeT await
  log $ "Iteratee: received first number: " <> show a
  b ← log "Iteratee: waiting for the second number..." *> MaybeT await
  log $ "Iteratee: received second number: " <> show b
  pure $ a + b

testPipe2 ∷ Effect Unit
testPipe2 = do
  _ /\ result ← pipe2 pass iteratee2
  log $ "Sum is: " <> show result
