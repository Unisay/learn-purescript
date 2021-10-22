module Transformers where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Identity.Trans (IdentityT(..))
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Homework.Todo (todo')

type Stack1 = ExceptT String (MaybeT Effect)

type Stack2 = MaybeT (ExceptT Int Effect)

type Stack3 = ReaderT String (MaybeT (ExceptT String (IdentityT Aff)))

testStack2 :: Stack2 Int
testStack2 = MaybeT <<< ExceptT <<< pure <<< Right $ Just 43

testStack1 :: forall a. a -> Stack1 a
testStack1 a = pure a

-- pure = ExceptT <<< MaybeT <<< pure <<< Just <<< Right

testStack1a :: Stack1 Int
testStack1a = ExceptT (MaybeT (pure (Just (Left "error"))))

testStack1b :: Stack1 Int
testStack1b = ExceptT (MaybeT (pure Nothing))

testStack3 :: forall a. a -> Stack3 a
testStack3 a = ReaderT \_ -> MaybeT (ExceptT (IdentityT (pure (Right (Just a)))))

testStack3a :: Aff Int -> Stack3 Int
testStack3a st = lift (lift (lift (lift st)))

runStack1 :: forall a. Stack1 a -> Effect (Maybe (Either String a))
runStack1 = todo' "implement"

runStack2 :: forall a. Stack2 a -> Effect (Either Int (Maybe a))
runStack2 = todo' "please implement"

runStack3 :: forall a. Stack3 a -> String -> Aff (Either String (Maybe a))
runStack3 = todo' "please implement"
