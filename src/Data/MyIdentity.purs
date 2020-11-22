module Data.MyIdentity where

import Data.Generic.Rep (class Generic)

data MyIdentity a
  = MyIdentity a

derive instance genericMyIdentity :: Generic (MyIdentity a) _
