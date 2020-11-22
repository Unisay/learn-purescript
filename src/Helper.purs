module Helper where

import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

class NotImplementedWarning

instance warn :: Warn (Text "not implemented") => NotImplementedWarning

notImplemented :: ∀ a. NotImplementedWarning => a
notImplemented = unsafeCoerce "Not implemented"
