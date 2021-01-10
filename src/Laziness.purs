module Laziness where

import Prelude
import Data.Custom.Thunk (Thunk, force, th)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Homework.Todo (todo)

{-

  Compiler uses rewrite rules (∀ a b) to evaluate expressions:

  a + b                     ->     (+) a b    ->   \a -> \b ->  a ?? b
  if true then a else _     ->     a
  if false then _ else b    ->     b
  a `add` b                 ->     add a b
  (\_ -> b) b               ->     b

  
  Strict evaluation sequence (How Purescript does it):
  
   1.   choice (2 + 2) (3 + 3) (1 == 1)     | 2 + 2 -> 4
   2.   choice 4 (3 + 3) (1 == 1)           | 3 + 3 -> 6
   3.   choice 4 6 (1 == 1)                 | 1 == 1 -> true
   4.   choice 4 6 true                     | choice   -> if _ then _ else _
   5.   if true then 6 else 4               | if true then a else b -> a
   6.   6          
   

  Thunk evaluation sequence (How Haskell does it):

   1.   choice (2 + 2) (3 + 3) (1 == 1)          
   2.   if 1 == 1 then 3 + 3 else 2 + 2     | choice   -> if _ then _ else _
   3.   if true then 3 + 3 else 2 + 2       | 1 == 1 -> true
   4.   if true then 6 else 2 + 2           | 3 + 3 -> 6
   5.   if true then 6 else _               | 
   6.   6                                   | if true then a else b -> a

  Thunk evaluation sequence done in Purescript:

   1.   choice (\_ -> 2 + 2) (\_ -> 3 + 3) (1 == 1) 
   2.   choice (\_ -> 2 + 2) (\_ -> 3 + 3) true 
   3.   if true then (\_ -> 3 + 3) else _
   4.   (\_ -> 3 + 3) 
   
   Needs to be finished (forced!)
   5.   (\_ -> 3 + 3) unit
   6.   3 + 3
   7.   6

-}
choice :: ∀ a. a -> a -> Boolean -> a
choice f t b = if b then t else f

choiceLazy :: ∀ a. Thunk a -> Thunk a -> Boolean -> Thunk a
choiceLazy f t b = if b then t else f

{-

forall a. 

Изоморфизм существует не только между типами a ~ Lazy a

но и между "ленивым выбором"  <-> "строгим выбором"

Строгий (Strict) выбор:
∀ a. a -> a -> Boolean -> a

Ленивый (Lazy) выбор:
∀ a. Thunk a -> Thunk a -> Boolean -> Thunk a

Доказательство (evidence):

-}
toLazyChoice ::
  ∀ a.
  (a -> a -> Boolean -> a) ->
  Thunk a -> Thunk a -> Boolean -> Thunk a
toLazyChoice f = todo "Homework"

fromLazyChoice ::
  ∀ a.
  (Thunk a -> Thunk a -> Boolean -> Thunk a) ->
  (a -> a -> Boolean -> a)
fromLazyChoice f = todo "Homework"

-- Homework
-- testThunk1 :: Unit -> Int
-- testThunk1 = choiceThunk heavyExpr (\_ -> 42) (2 + 2 == 4)
-- testThunk2 :: Unit -> Int
-- testThunk2 = choiceThunk heavyExpr (\_ -> 42) (2 + 2 == 5)
heavyExpr :: Thunk Int
heavyExpr = th \_ -> go 0
  where
  go i =
    if i < 1000 then
      go (i + 1)
    else
      let
        _ = unsafePerformEffect (log $ "Heavy calculation -> 1000")
      in
        i

-- | Uses value if its available or defalut value (provided lazily, by-need)
-- |
-- | ```purescript
-- | > Nothing `alt` th \_ -> 42
-- | 42
-- | 
-- | > Just 100 `alt` th \_ -> 42
-- | 100
-- | ```
alt :: forall a. Maybe a -> Thunk a -> a
alt ma d = todo "Homework"
