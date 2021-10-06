module Data.Tracer where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Homework.Todo (todo')

data Tracer a = Tracer (Array Tr) a

instance showTracer :: Show (Tracer a) where
  show = renderTracer

derive instance functorTracer :: Functor Tracer

instance applyTracer :: Apply Tracer where
  apply :: forall a b. Tracer (a -> b) -> Tracer a -> Tracer b
  apply = todo' "Implement"

instance applicativeTracer :: Applicative Tracer where
  pure :: forall a. a -> Tracer a
  pure = Tracer []

instance bindTracer :: Bind Tracer where
  bind :: forall a b. Tracer a -> (a -> Tracer b) -> Tracer b
  bind = todo' "Implement"

-- instance semigroupTracer :: Semigroup (Tracer a)

data Tr = Tr String | Clear

derive instance genericTr :: Generic Tr _

instance showTr :: Show Tr where
  show = genericShow

tr :: forall a. Tr -> a -> Tracer a
tr t a = Tracer [ t ] a

tr_ :: Tr -> Tracer Unit
tr_ t = tr t unit

trace :: String -> Tracer Unit
trace = tr_ <<< Tr

clear :: Tracer Unit
clear = tr_ Clear

runTracerResult :: forall a. Tracer a -> a
runTracerResult (Tracer _trs a) = a

runTracerLog :: forall a. Tracer a -> Array Tr
runTracerLog (Tracer trs _a) = trs

renderTracer :: forall a. Tracer a -> String
renderTracer = joinWith "\n" <<< map show <<< runTracerLog

--------------------------------------------------------------------------------

hadoinkel :: Tracer Int
hadoinkel = do
  h <- hapax
  s <- spadoinkle
  trace "Hapax Joins Spadoinkle. Hadoinkel emerges!"
  pure (h + s)

legomenon :: Tracer Int
legomenon = do
  clear
  trace "Legomenon kills Hadoinkel and takes its assets!"
  h <- hadoinkel
  trace $ "I am Legomenon! [" <> show h <> "]"
  pure h

hapax :: Tracer Int
hapax = trace "I am Hapax! [42]" $> 42

spadoinkle :: Tracer Int
spadoinkle = trace "I am Spadoinkle! [11]" *> pure 11
