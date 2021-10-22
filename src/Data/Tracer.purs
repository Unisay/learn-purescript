module Data.Tracer where

import Prelude

import Data.Newtype (class Newtype, over, unwrap, wrap)
import Control.Apply (lift2)
import Data.Array (foldl)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

newtype TracerT (m :: Type -> Type) a = TracerT (m (Tuple (Array Tr) a))

liftTracerT :: forall m a. Functor m => m a -> TracerT m a
liftTracerT ma = TracerT $ map pure ma

liftTracerTuple :: forall m a. Applicative m => Tuple (Array Tr) a -> TracerT m a
liftTracerTuple = pure >>> wrap

type Tracer a = TracerT Identity a

derive instance genericTracerT :: Generic (TracerT m a) _

derive instance newtypeTracerT :: Newtype (TracerT m a) _

instance (Show (m String), Functor m, Show a) => Show (TracerT m a) where
  show = show <<< map show <<< unwrap

instance Functor m => Functor (TracerT m) where
  map = over TracerT <<< map <<< map

instance Apply m => Apply (TracerT m) where
  apply f a = wrap (lift2 apply (unwrap f) (unwrap a))

instance Applicative m => Applicative (TracerT m) where
  pure = wrap <<< pure <<< pure

instance (Applicative m, Bind m) => Bind (TracerT m) where
  bind t f =
    wrap do
      Tuple trsl a <- unwrap t
      Tuple trsr b <- unwrap $ f a
      pure $ Tuple (trsl <> trsr) b

data Tr = Tr String | Clear

derive instance genericTr :: Generic Tr _

instance showTr :: Show Tr where
  show = genericShow

tr :: forall m a. Applicative m => Tr -> a -> TracerT m a
tr t a = TracerT $ pure $ Tuple [ t ] a

tr_ :: forall m. Applicative m => Tr -> TracerT m Unit
tr_ t = tr t unit

trace :: forall m. Applicative m => String -> TracerT m Unit
trace = tr_ <<< Tr

clear :: forall m. Applicative m => TracerT m Unit
clear = tr_ Clear

runTracerTResult :: forall m a. Functor m => TracerT m a -> m a
runTracerTResult (TracerT m) = snd <$> m

runTracerTLog :: forall m a. Functor m => TracerT m a -> m (Array Tr)
runTracerTLog (TracerT m) = fst <$> m

runTracerLog :: forall a. Tracer a -> Array Tr
runTracerLog = runTracerTLog >>> unwrap

renderTracerT :: forall m a. Functor m => TracerT m a -> m String
renderTracerT = map (joinWith "\n" <<< map show) <<< runTracerTLog

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

