module Foreigns where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Foreign (F, Foreign, readInt, readString, renderForeignError)
import Foreign.Index (ix)
import Homework.Todo (todo)

foreign import point ∷ Foreign

type Point = { x ∷ Int, y ∷ Int }

runF ∷ ∀ a. F a → Either String a
runF ft = case runExcept ft of
  Left errors → Left $ fold $ map renderForeignError errors
  Right a → Right a

readPoint ∷ Either String Point
readPoint = runF ado
  x ← point `ix` 1 >>= readInt
  y ← point `ix` 0 >>= readInt
  in { x, y }

foreign import alist0 ∷ Foreign
foreign import alist1 ∷ Foreign
foreign import alistS ∷ Foreign

data AList (a ∷ Type) (b ∷ Type) = ACons a (AList a b) | AEnd b

derive instance genericAList ∷ Generic (AList a b) _

-- instance showAListCharString ∷ Show (AList Char String) where
--   show = case _ of
--     ACons h t → CodeUnit.singleton h <> show t
--     AEnd b → b
instance showAListPoly ∷ (Show a, Show b) ⇒ Show (AList a b) where
  show = case _ of
    ACons h t → show h <> ":" <> show t
    AEnd b → show b

boom ∷ AList Char String
boom = ACons 'b' (ACons 'o' (AEnd "om"))

readAListCharString ∷ Foreign → Either String (AList Char String)
readAListCharString f = readAListConsChar f <|> readAListEndString f

readAListEndString ∷ Foreign → Either String (AList Char String)
readAListEndString f' = AEnd <$> runF (readString f')

readAListConsChar ∷ Foreign → Either String (AList Char String)
readAListConsChar _f = ACons <$> readHead <*> readTail
  where
  readHead = todo "please implement"
  readTail = todo "please implement"
