module Data.MyFree where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Motsunabe (Doc, pretty)

data Free f a = Pure a | Bind (f (Free f a))

instance Functor f ⇒ Functor (Free f) where
  map ∷ ∀ a b. (a → b) → Free f a → Free f b
  map g =
    case _ of
      Pure a → Pure (g a)
      Bind f → Bind (map (map g) f)

instance Functor f ⇒ Apply (Free f) where
  apply ∷ ∀ a b. Free f (a → b) → Free f a → Free f b
  apply f m =
    case f of
      Pure g → g <$> m
      Bind g → Bind (flip apply m <$> g)

instance Functor f ⇒ Applicative (Free f) where
  pure ∷ ∀ a. a → Free f a
  pure = Pure

instance Functor f ⇒ Bind (Free f) where
  bind ∷ ∀ a b. Free f a → (a → Free f b) → Free f b
  bind m g =
    case m of
      Pure a → g a
      Bind f → Bind (flip bind g <$> f)

instance Functor f ⇒ Monad (Free f)

liftF ∷ ∀ f a. Functor f ⇒ f a → Free f a
liftF = Bind <<< map pure

foldFree ∷ ∀ f m a. Monad m ⇒ (f ~> m) → Free f a → m a
foldFree nt = case _ of
  Pure a → pure a
  Bind f → nt f >>= foldFree nt

showFree ∷ ∀ f a. (f ~> Writer Doc) → Free f a → String
showFree w = foldFree w >>> execWriter >>> pretty 100
