module Data.Tracer where

import Prelude

import Data.Array (foldl)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (log)

data Tracer a = Tracer (Array Tr) a

derive instance genericTracer :: Generic (Tracer a) _

instance showTracer :: Show a => Show (Tracer a) where
  show = genericShow

derive instance functorTracer :: Functor Tracer

instance applyTracer :: Apply Tracer where
  apply :: forall a b. Tracer (a -> b) -> Tracer a -> Tracer b
  apply (Tracer ft f) (Tracer at a) = Tracer (ft <> at) (f a)

instance applicativeTracer :: Applicative Tracer where
  pure :: forall a. a -> Tracer a
  pure = Tracer []

instance bindTracer :: Bind Tracer where
  bind :: forall a b. Tracer a -> (a -> Tracer b) -> Tracer b
  bind (Tracer at a) f = case f a of Tracer bt b -> Tracer (at <> bt) b

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

logTracer :: forall a. Show a => Tracer a -> Effect Unit
logTracer t = traverse_ log (foldl next Nothing (runTracerLog t))
  where
  next :: Maybe String -> Tr -> Maybe String
  next acc = case _ of
    Tr msg -> Just $ maybe msg (\s -> s <> "\n" <> msg) acc
    Clear -> Nothing

--------------------------------------------------------------------------------

{-

> runTracerLog hapax
[(Tr "I am Hapax! [42]")]

> logTracer hapax
I am Hapax! [42]

> spadoinkle 
(Tr "I am Spadoinkle! [11]")

> logTracer spadoinkle 
I am Spadoinkle! [11]

> hadoinkel 
(Tr "I am Hapax! [42]")
(Tr "I am Spadoinkle! [11]")
(Tr "Hapax joins Spadoinkle. Hadoinkel [53] emerges!")

> logTracer hadoinkel    
I am Hapax! [42]
I am Spadoinkle! [11]
Hapax joins Spadoinkle. Hadoinkel [53] emerges!

> legomenon 
(Tr "I am Hapax! [42]")
(Tr "I am Spadoinkle! [11]")
(Tr "Hapax joins Spadoinkle. Hadoinkel [53] emerges!")
Clear
(Tr "Legomenon kills Hadoinkel and takes its assets!")
(Tr "I am Legomenon! [53]")

> logTracer legomenon 
Legomenon kills Hadoinkel and takes its assets!
I am Legomenon! [53]
-}

hadoinkel :: Tracer Int
hadoinkel = do
  h <- hapax
  s <- spadoinkle
  let hs = h + s
  trace $ "Hapax joins Spadoinkle. Hadoinkel [" <> show hs <> "] emerges!"
  pure hs

legomenon :: Tracer Int
legomenon = do
  power <- hadoinkel
  if power < 10 then do
    clear
    trace "Legomenon kills Hadoinkel and takes its power!"
    trace $ "I am Legomenon! [" <> show power <> "]"
    pure power
  else
    trace "Legomenon dies because Hadoinkel is too strong!" $> power

hapax :: Tracer Int
hapax = trace "I am Hapax!" $> 4

spadoinkle :: Tracer Int
spadoinkle = trace "I am Spadoinkle!" *> pure 1

