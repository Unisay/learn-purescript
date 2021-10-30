module ConfigurableTracer where

import Prelude

import Data.Array as Array
import Data.Config (Config(..), config)
import Data.Tracer (Tr(..), Tracer, TracerT, clear, liftTracerT, tr, tr_)

type TinC r a = TracerT (Config r) a

tracerInConfig :: TracerT (Config { n :: Int }) Int
tracerInConfig = do
  { n } <- liftTracerT config
  clear
  _ <- replicate n $ tr_ (Tr ("Configured Tracer: " <> show n))
  pure n

tracerInConfigC :: Int -> TracerT (Config { n :: Int }) Unit
tracerInConfigC x = do
  { n } <- liftTracerT config
  tr_ (Tr ("Configured Tracer: " <> show (n + x)))

composedTracersInConfigB :: TinC { n :: Int } Unit
composedTracersInConfigB = do
  n <- tracerInConfig
  tracerInConfigC n

configInTracer :: Tracer (Config { n :: Int } Unit)
configInTracer = tr (Tr "Traced Config") (Config \_configIgnored -> unit)

replicate :: forall f a. Apply f => Int -> f a -> f (Array a)
replicate n fa
  | n <= 1 = map pure fa
  | otherwise = Array.cons <$> fa <*> replicate (n - 1) fa
