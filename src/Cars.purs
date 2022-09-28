module Cars where

import Custom.Prelude

import Data.Machine.Mealy (MealyT, Step(..), mealy)
import Effect (Effect)

machine ∷ MealyT Effect Int String
machine = mealy \i → pure $ Emit (show i) machine
