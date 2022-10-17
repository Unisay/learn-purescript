module Control.Coroutine.Iteratee where

import Custom.Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)

newtype Iteratee a m r = Iteratee (m (Either (a → Iteratee a m r) r))

bounceIter ∷ ∀ a m r. Iteratee a m r → m (Either (a → Iteratee a m r) r)
bounceIter (Iteratee m) = m

instance Functor m ⇒ Functor (Iteratee a m) where
  map f (Iteratee m) = Iteratee (bimap (map (map f)) f <$> m)

instance Monad m ⇒ Apply (Iteratee a m) where
  apply = ap

instance Monad m ⇒ Applicative (Iteratee a m) where
  pure = Iteratee <<< pure <<< Right

instance Monad m ⇒ Bind (Iteratee a m) where
  bind iter f = Iteratee do
    bounceIter iter >>= case _ of
      Left k → pure $ Left $ bindFlipped f <$> k
      Right r → bounceIter $ f r

instance Monad m ⇒ Monad (Iteratee a m)

instance MonadTrans (Iteratee a) where
  lift = Iteratee <<< liftA1 Right

instance MonadEffect m ⇒ MonadEffect (Iteratee a m) where
  liftEffect = lift <<< liftEffect

await ∷ ∀ a m. Monad m ⇒ Iteratee a m a
await = Iteratee $ pure $ Left pure

runIteratee ∷ ∀ a m x. MonadRec m ⇒ Array a → Iteratee a m x → m x
runIteratee = tailRecM2 \is it → bounceIter it <#>
  case Array.uncons is of
    Nothing →
      case _ of
        Left k → Loop { a: [], b: k (unsafeThrow "No more inputs") }
        Right x → Done x
    Just { head, tail } →
      case _ of
        Left k → Loop { a: tail, b: k head }
        Right x → Done x

--------------------------------------------------------------------------------
-- Tests -----------------------------------------------------------------------

test ∷ Effect Unit
test = runIteratee [ 11, 42 ] iteratee

iteratee ∷ Iteratee Int Effect Unit
iteratee = do
  log "Enter two numbers: "
  a ← await
  log $ "First number: " <> show a
  b ← await
  log $ "Second number: " <> show b
  log $ "Sum of two numbers is " <> show (a + b)

