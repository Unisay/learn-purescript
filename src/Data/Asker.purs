module Data.Asker where

import Prelude

import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

newtype Asker r a = Asker (Array r -> Tuple (Array r) (Array a))

derive instance functorAsker :: Functor (Asker r)

instance applyAsker :: Apply (Asker r) where
  apply :: forall a b. Asker r (a -> b) -> Asker r a -> Asker r b
  apply (Asker fs) (Asker as) = Asker \rs ->
    case fs rs of
      Tuple rs' abs ->
        case as rs' of
          Tuple rs'' aa ->
            Tuple rs'' ado
              f <- abs
              a <- aa
              in f a

instance applicativeAsker :: Applicative (Asker r) where
  pure a = Asker \rs -> Tuple rs (pure a)

ask :: forall r. Asker r r
ask = Asker \r ->
  case Array.uncons r of
    Just { head, tail } -> Tuple tail [ head ]
    Nothing -> Tuple r r

runAsker :: forall r a. Monoid a => Asker r a -> Array r -> a
runAsker (Asker f) answers = case f answers of
  Tuple _unconsumedAnswers as -> Array.fold as

--------------------------------------------------------------------------------

programInAsker :: forall a. Semigroup a => Asker a a
programInAsker = ado
  a <- ask
  b <- ask
  in a <> b
