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

data Tr = Begin | Action String | End | Clear

derive instance genericTr :: Generic Tr _

instance showTr :: Show Tr where
  show = genericShow

tr :: Tr -> Tracer Unit
tr t = Tracer [ t ] unit

runTracerResult :: forall a. Tracer a -> a
runTracerResult (Tracer _trs a) = a

runTracerLog :: forall a. Tracer a -> Array Tr
runTracerLog (Tracer trs _a) = trs

renderTracer :: forall a. Tracer a -> String
renderTracer = joinWith "\n" <<< map show <<< runTracerLog

