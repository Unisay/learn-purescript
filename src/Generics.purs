module Generics where

import Data.Generic.Rep as G
import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)


data User = MkUser String String String

derive instance G.Generic User _

data POS = Product | Sum | Neither

instance Show POS where 
  show = case _ of
    Product -> "Product"
    Sum -> "Sum"
    Neither -> "Neither"

class ProductOrSum a where
  productOrSum :: a -> POS

instance ProductOrSum Unit where productOrSum _ = Neither
instance ProductOrSum Int where productOrSum _ = Sum
instance ProductOrSum String where productOrSum _ = Sum
instance ProductOrSum Boolean where productOrSum _ = Sum
instance ProductOrSum (Tuple a b) where productOrSum _ = Product
instance ProductOrSum (Maybe a) where productOrSum _ = Sum
instance ProductOrSum (Either a b) where productOrSum _ = Sum


genericProductOrSum :: 
  forall t rep . 
  G.Generic t rep  => 
  ProductOrSum rep => 
  t -> POS
genericProductOrSum = productOrSum <<< G.from 


instance ProductOrSum (G.Product a b) where productOrSum _ = Product
instance ProductOrSum (G.Sum a b) where productOrSum _ = Sum
instance ProductOrSum t => ProductOrSum (G.Constructor s t) 
   where productOrSum (G.Constructor t) = productOrSum t

data League = Premier User | Bundes User | LA User String

derive instance G.Generic League _
