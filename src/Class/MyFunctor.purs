module Class.MyFunctor where

import Data.Generic.Rep (class Generic)
import Helper (notImplemented)

class MyFunctor f where
  fmap :: forall a b. (a -> b) -> f a -> f b
