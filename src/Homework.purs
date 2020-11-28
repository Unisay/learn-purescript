module Homework where

import Prelude
import Data.Tuple
import Partial.Unsafe (unsafeCrashWith)
import Data.String.CodePoints (codePointFromChar)
import Data.String as String

-- Syntactic shugar
type T
  = Tuple

-- Implement functions with "todo"
--
-- | `mapFst` changes first component of a Tuple ("maps" over first component)
-- | leaving second one untouched.
--
-- Can test it in REPL with: 
-- mapFst gt5 (Tuple 42 'x')
mapFst :: forall a b c. (a -> c) -> T a b -> T c b
mapFst f (Tuple x y) = Tuple (f x) y

mapFst' :: forall a b c. (a -> c) -> T a b -> T c b
mapFst' f (Tuple a b) = swap (mapSnd f (swap (Tuple a b)))

-- | Try to implement in terms of mapFst + something we learned before
mapSnd :: forall a b c. (b -> c) -> T a b -> T a c
mapSnd f (Tuple x y) = Tuple x (f y)

mapSnd' :: forall a b c. (b -> c) -> T a b -> T a c
mapSnd' f (Tuple a b) = swap (mapFst f (swap (Tuple a b)))

-- | Given two functions, `bimap` applies them to tuple components correspondingly.
-- Can test it in REPL with: 
-- bimap gt5 c2s (Tuple 42 'x')
bimap :: forall a b c d. (a -> b) -> (c -> d) -> T a c -> T b d
bimap f g (Tuple a c) = Tuple (f a) (g c)

bimap' :: forall a b c d. (a -> b) -> (c -> d) -> T a c -> T b d
bimap' f g (Tuple a c) = (mapSnd' g (mapFst' f (Tuple a c)))

-- | Given two functions such that both expect argument of the same type,
-- applies both of them to the same argument and joins reulsts in a tuple.
fanOut :: forall a b c. (a -> b) -> (a -> c) -> a -> T b c
fanOut f g a = (mapSnd' g (mapFst' f (Tuple a a)))

-- Utilities:
todo :: forall a. String -> a
todo message = unsafeCrashWith $ "NOT IMPLEMENTED: " <> message

-- You can use this sample functions for testing in REPL:
gt5 :: Int -> Boolean
gt5 x = x >= 5

i2s :: Int -> String
i2s = case _ of
  42 -> "cool"
  otherwise -> "not cool"

c2s :: Char -> String
c2s = codePointFromChar >>> String.singleton
