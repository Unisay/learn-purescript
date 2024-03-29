module Homework.Generics where

import Prelude
import Data.Maybe (Maybe)

class HasDepth a where
  depth :: a -> Int

data Shallow a
  = Shallow
  | Deeper a
  | Deep (Maybe a)

shallowThing :: Shallow Unit
shallowThing = Shallow

{-

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

The task is to implement depth:

- write instances for "standard" types
- write `genericDepth` for all types that have a Generic instance (e.g. `Shallow`)
- write instance for Shallow that uses `genericDepth`

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
