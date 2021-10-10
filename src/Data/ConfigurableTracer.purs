module ConfigurableTracer where

import Prelude

import Data.Array as Array
import Data.Config (Config(..), runConfig)
import Data.Tracer (Tr(..), Tracer, clear, renderTracer, tr, tr_)
import Homework.Todo (todo')

newtype TinC r a = TinC (Config r (Tracer a))

instance functorTinC :: Functor (TinC r) where
  map = todo' "Implement"

instance applyTinC :: Apply (TinC r) where
  apply = todo' "Implement"

instance applicativeTinC :: Applicative (TinC r) where
  pure = todo' "Implement"

instance bindTinC :: Bind (TinC r) where
  bind = todo' "Implement"

tracerInConfig :: Config { n :: Int } (Tracer Int)
tracerInConfig = Config \{ n } -> do
  clear
  _ <- replicate n $ tr_ (Tr ("Configured Tracer: " <> show n))
  pure n

tracerInConfigC :: Int -> Config { n :: Int } (Tracer Unit)
tracerInConfigC x = Config \{ n } ->
  tr_ (Tr ("Configured Tracer: " <> show (n + x)))

composedTracersInConfigA :: TinC { n :: Int } Unit
composedTracersInConfigA = TinC tracerInConfig *> TinC (tracerInConfigC 3)

composedTracersInConfigB :: TinC { n :: Int } Unit
composedTracersInConfigB = do
  n <- TinC tracerInConfig
  TinC $ tracerInConfigC n

runTracerInConfig :: forall r a. Config r (Tracer a) -> r -> String
runTracerInConfig c = renderTracer <<< runConfig c

configInTracer :: Tracer (Config { n :: Int } Unit)
configInTracer = tr (Tr "Traced Config") (Config \_configIgnored -> unit)

replicate :: forall f a. Apply f => Int -> f a -> f (Array a)
replicate n fa
  | n <= 1 = map pure fa
  | otherwise = Array.cons <$> fa <*> replicate (n - 1) fa
