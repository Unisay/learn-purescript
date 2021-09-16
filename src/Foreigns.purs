module Foreigns where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Foreign (F, Foreign, readInt, renderForeignError)
import Foreign.Index ((!))

foreign import point :: Foreign

type Point = { x :: Int, y :: Int }

runF :: forall a. F a -> Either String a
runF ft = case runExcept ft of
  Left errors -> Left $ fold $ map renderForeignError errors
  Right a -> Right a

readPoint :: Either String Point
readPoint = runF ado
  x <- point ! 1 >>= readInt
  y <- point ! 0 >>= readInt
  in { x, y }

