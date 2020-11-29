module Homework.Todo where

import Prelude
import Partial.Unsafe (unsafeCrashWith)

todo :: ∀ a. String -> a
todo message = unsafeCrashWith $ "NOT IMPLEMENTED: " <> message

todo' :: ∀ a b. String -> a -> b
todo' msg _ = todo msg
