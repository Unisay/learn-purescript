module Data.Coroutine.Generator where

import Custom.Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)

newtype Generator a m r = Generator (m (Either (a /\ Generator a m r) r))

bounceGen ∷ ∀ a m r. Generator a m r → m (Either (a /\ Generator a m r) r)
bounceGen (Generator m) = m

instance Functor m ⇒ Functor (Generator a m) where
  map f (Generator m) = Generator (bimap (map (map f)) f <$> m)

instance Monad m ⇒ Apply (Generator a m) where
  apply = ap

instance Monad m ⇒ Applicative (Generator a m) where
  pure = Generator <<< pure <<< Right

instance Monad m ⇒ Bind (Generator a m) where
  bind gen f = Generator do
    bounceGen gen >>= case _ of
      Left (a /\ t) → pure $ Left $ a /\ bind t f
      Right r → bounceGen $ f r

instance Monad m ⇒ Monad (Generator a m)

instance MonadTrans (Generator a) where
  lift = Generator <<< liftA1 Right

instance MonadEffect m ⇒ MonadEffect (Generator a m) where
  liftEffect = lift <<< liftEffect

yield ∷ ∀ a m. Monad m ⇒ a → Generator a m Unit
yield a = Generator $ pure $ Left $ a /\ pass

runGenerator ∷ ∀ a m x. MonadRec m ⇒ Generator a m x → m (Array a /\ x)
runGenerator = go identity
  where
  go = tailRecM2 \f g →
    bounceGen g <#> case _ of
      Left (a /\ cont) → Loop { a: f <<< Array.cons a, b: cont }
      Right x → Done $ f [] /\ x

--------------------------------------------------------------------------------
-- Tests -----------------------------------------------------------------------

test ∷ Effect Unit
test = do
  collectedValues ← runGenerator generator
  logShow collectedValues

generator ∷ Generator Int Effect Int
generator = do
  log "Yielding one"
  yield one
  log "Yielding two"
  yield 2
  log "Returning three"
  pure 3
