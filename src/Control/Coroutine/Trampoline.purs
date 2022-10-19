module Control.Coroutine.Trampoline where

import Custom.Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)

newtype Trampoline m r = Trampoline (m (Trampoline m r \/ r))

bounce ∷ ∀ m r. Trampoline m r → m (Either (Trampoline m r) r)
bounce (Trampoline m) = m

instance Functor m ⇒ Functor (Trampoline m) where
  map f (Trampoline m) = Trampoline (bimap (map f) f <$> m)

instance Monad m ⇒ Apply (Trampoline m) where
  apply f a = mzipWith ($) f a

instance Monad m ⇒ Applicative (Trampoline m) where
  pure = Trampoline <<< pure <<< Right

instance Monad m ⇒ Bind (Trampoline m) where
  bind (Trampoline m) f = Trampoline do
    m >>= case _ of
      Left t → pure $ Left $ bind t f
      Right r → bounce $ f r

instance Monad m ⇒ Monad (Trampoline m)

instance MonadTrans Trampoline where
  lift = Trampoline <<< liftA1 Right

instance MonadEffect m ⇒ MonadEffect (Trampoline m) where
  liftEffect = lift <<< liftEffect

pause ∷ ∀ m. Monad m ⇒ Trampoline m Unit
pause = Trampoline $ pure $ Left pass

debugPause ∷ ∀ m. MonadEffect m ⇒ String → Trampoline m Unit
debugPause s = Trampoline $ log s $> Left pass

run ∷ ∀ m r. Monad m ⇒ Trampoline m r → m r
run t = bounce t >>= either run pure

mzipWith
  ∷ ∀ a b c m
  . Monad m
  ⇒ (a → b → c)
  → Trampoline m a
  → Trampoline m b
  → Trampoline m c
mzipWith f t1 t2 = Trampoline (zip <$> bounce t1 <*> bounce t2)
  where
  zip = case _, _ of
    Left a, Left b → Left (mzipWith f a b)
    Left a, Right b → Left (mzipWith f a (pure b))
    Right a, Left b → Left (mzipWith f (pure a) b)
    Right a, Right b → Right (f a b)

interleave ∷ ∀ m r. Monad m ⇒ Array (Trampoline m r) → Trampoline m (Array r)
interleave = Array.foldr (mzipWith Array.cons) (pure [])

--------------------------------------------------------------------------------
--- Tests ----------------------------------------------------------------------

thread1 ∷ Trampoline Effect Unit
thread1 = do
  pause
  log "1: World!"
  pause

thread2 ∷ Trampoline Effect Unit
thread2 = do
  log "2: Hello, "
  pause
  pause

thread3 ∷ Trampoline Effect Unit
thread3 = do
  pause
  pause
  log "3: BRUH "

threads ∷ Trampoline Effect (Array Unit)
threads = interleave [ thread3, thread1, thread2 ]

renderTrampoline ∷ ∀ m r. MonadRec m ⇒ Show r ⇒ Trampoline m r → m String
renderTrampoline = mempty # tailRecM2 \a b →
  bounce b <#> case _ of
    Right r → Done $ foldWrap $ wrin ("Effect -> Result (" <> show r <> ")") a
    Left k → Loop { a: wrap "Effect -> Pause -> (" ")" a, b: k }

{-
    > renderTrampoline threads
    2: Hello, 
    1: World!
    "Effect -> Pause -> (Effect -> Result ([unit,unit]))"
-}

--------------------------------------------------------------------------------
--- Homework -------------------------------------------------------------------

threadNums ∷ Trampoline Effect Unit
threadNums = do
  log "1"
  log "2"
  pause
  log "4"
  pause
  pause
  log "7"
  log "8"
  pause

threadFoo ∷ Trampoline Effect Unit
threadFoo = do
  pause
  log "3: foo"
  pause
  pause
  log "6: foo"
  pause
  log "9: foo"
  pause

threadBar ∷ Trampoline Effect Unit
threadBar = do
  pause
  pause
  pause
  log "5: bar"
  pause
  pause
  pause
  log "10: bar"

threadToBeContinued ∷ Trampoline Effect Unit
threadToBeContinued = do
  pause
  pause
  pause
  pause
  pause
  pause
  pause
  pause
  log "To be continued..."

threadFooBar ∷ Trampoline Effect (Array Unit)
threadFooBar = interleave
  [ threadToBeContinued, threadBar, threadFoo, threadNums ]

--------------------------------------------------------------------------------
--- Wrap -----------------------------------------------------------------------

data Wrap s = Wrap (Array s) s (Array s)

derive instance Functor Wrap

instance Monoid s ⇒ Semigroup (Wrap s) where
  append w1 w2 = Wrap mempty (foldWrap w1 <> foldWrap w2) mempty

instance Monoid s ⇒ Monoid (Wrap s) where
  mempty = Wrap mempty mempty mempty

wrap ∷ ∀ s. s → s → Wrap s → Wrap s
wrap prefix suffix (Wrap before focus after) =
  Wrap (Array.cons prefix before) focus (Array.snoc after suffix)

withFocus ∷ ∀ s. (s → s) → Wrap s → Wrap s
withFocus f (Wrap p a s) = Wrap p (f a) s

wrin ∷ ∀ s. s → Wrap s → Wrap s
wrin = withFocus <<< const

foldWrap ∷ ∀ s. Monoid s ⇒ Wrap s → s
foldWrap (Wrap prefix s suffix) = fold prefix <> s <> fold suffix
