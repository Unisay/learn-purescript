module Data.MyConst where

import Data.Generic.Rep (class Generic)

data MyConst c a
  = MyConst c

derive instance genericMyConst :: Generic (MyConst c a) _
