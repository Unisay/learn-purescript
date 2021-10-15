module ConfigurableTracer where

import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Config (Config(..))
import Data.Identity (Identity)
import Data.Newtype (class Newtype, wrap, unwrap, over)
import Data.Tracer (Tr(..), TracerT(..), clear, tr, tr_)

type TinC r = TracerT (Config r)

tracerInConfig :: Config { n :: Int } (Tracer Int)
tracerInConfig = Config \{ n } -> do
  clear
  _ <- replicate n $ tr_ (Tr ("Configured Tracer: " <> show n))
  pure n

tracerInConfigC :: Int -> Config { n :: Int } (Tracer Unit)
tracerInConfigC x = Config \{ n } ->
  tr_ (Tr ("Configured Tracer: " <> show (n + x)))

composedTracersInConfigA :: TinC { n :: Int } Unit
composedTracersInConfigA = wrap tracerInConfig *> wrap (tracerInConfigC 3)

composedTracersInConfigB :: TinC { n :: Int } Unit
composedTracersInConfigB = do
  n <- wrap tracerInConfig
  wrap $ tracerInConfigC n

configInTracer :: Tracer (Config { n :: Int } Unit)
configInTracer = tr (Tr "Traced Config") (Config \_configIgnored -> unit)

replicate :: forall f a. Apply f => Int -> f a -> f (Array a)
replicate n fa
  | n <= 1 = map pure fa
  | otherwise = Array.cons <$> fa <*> replicate (n - 1) fa
