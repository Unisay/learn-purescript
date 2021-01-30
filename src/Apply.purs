module Apply where

import Prelude
import Homework.Todo (todo)

invert :: forall f. Functor f => f Boolean -> f Boolean
invert _ = todo "Implement"

addF :: forall f. Functor f => f Int -> f Int -> f Int
addF _ _ = todo "Try to implement"
