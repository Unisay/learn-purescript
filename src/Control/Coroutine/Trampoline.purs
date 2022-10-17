module Control.Coroutine.Trampoline where

import Custom.Prelude

import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either.Nested (type (\/))
import Data.Function as Function
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)

newtype Trampoline m r = Trampoline (m (Trampoline m r \/ r))

bounce ∷ ∀ m r. Trampoline m r → m (Either (Trampoline m r) r)
bounce (Trampoline m) = m

instance Functor m ⇒ Functor (Trampoline m) where
  map f (Trampoline m) = Trampoline (bimap (map f) f <$> m)

instance Applicative m ⇒ Apply (Trampoline m) where
  apply trf tra = Trampoline ado
    f ← bounce trf
    a ← bounce tra
    in
      case f, a of
        Left tf, Left ta → Left $ apply tf ta
        Left tf, Right pa → Left $ map (Function.applyFlipped pa) tf
        Right pf, Left ta → Left $ map pf ta
        Right pf, Right pa → Right $ pf pa

instance Applicative m ⇒ Applicative (Trampoline m) where
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
-- Tests -----------------------------------------------------------------------

hello ∷ Trampoline Effect Unit
hello = do
  log "Hello, "
  pause
  log "World!"

ok ∷ Trampoline Effect Unit
ok = do
  log "its ok!"
