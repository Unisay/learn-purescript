module Homework.Generics where

import Prelude

import Data.Array (zipWith)
import Data.Either (Either(..), either)
import Data.Foldable (maximum)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Product(..), Sum(..), from)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

class HasDepth a where
  depth :: a -> Int

data Shallow a = Shallow | Deeper a | Deep (Maybe a)

shallowThing :: Shallow Unit
shallowThing = Shallow

tests :: Array Boolean
tests = 
  zipWith eq
    [ depth shallowThing  
    , depth unit 
    , depth (Deeper unit) 
    , depth (Deep (Nothing :: Maybe Unit)) 
    , depth (Deep (Just unit)) 
    , depth (Tuple unit unit) 
    , depth (Left "ok" :: Either String Unit) 
    , depth (Just "ok") 
    , depth (Just [Left "Error", Right (Tuple (Deep (Just "yes")) 42)]) 
    ]
    [ 0 , 1 , 2 , 1 , 3 , 1 , 2 , 2 , 5 ]

{-

The task is to implement depth using generics 
such that it works for any type (example in REPL):

> depth shallowThing 
0

> depth unit         
1

> depth (Deeper unit)
2

> depth (Deep (Nothing :: Maybe Unit))
1

> depth (Deep (Just unit))              
3

> depth (Tuple unit unit)
1

> depth (Left "ok" :: Either String Unit)
2

> depth (Just "ok")                      
2

> depth $ Just [Left "Error", Right (Tuple (Deep (Just "yes")) 42)]
5

> import Data.Foldable (all)

> tests    
[true,true,true,true,true,true,true,true,true]

> all (eq true) tests
true

-}

derive instance Generic (Shallow a) _

instance Show a => Show (Shallow a) where 
  show = genericShow

instance HasDepth Void where depth _ = 0

instance HasDepth Unit where depth _ = 1

instance HasDepth Int where depth _ = 1

instance HasDepth String where depth _ = 1

instance HasDepth Boolean where depth _ = 1

instance HasDepth a => HasDepth (Maybe a) where 
  depth = maybe 0 (depth >>> (_ + 1))

instance (HasDepth a, HasDepth b) => HasDepth (Either a b) where 
  depth = either step step 
    where 
      step :: forall x. HasDepth x => x -> Int
      step = depth >>> (_ + 1)

instance (HasDepth a, HasDepth b) => HasDepth (Tuple a b) where 
  depth (Tuple a b) = max (depth a) (depth b)

instance HasDepth a => HasDepth (Array a) where 
  depth = fromMaybe 0 <<< maximum <<< map depth
  
instance HasDepth a => HasDepth (Shallow a) where 
  depth = genericDepth

instance (HasDepth a, HasDepth b) => HasDepth (Product a b) where 
  depth (Product a b) = max (depth a) (depth b)
  
instance (HasDepth a, HasDepth b) => HasDepth (Sum a b) where 
  depth (Inl a) = depth a
  depth (Inr b) = depth b

instance HasDepth NoArguments where 
  depth _ = 0

instance HasDepth a => HasDepth (Argument a) where 
  depth (Argument a) = 1 + depth a

instance HasDepth t => HasDepth (Constructor s t) 
   where depth (Constructor t) = depth t

genericDepth :: forall t rep. Generic t rep => HasDepth rep => t -> Int
genericDepth = depth <<< from 
