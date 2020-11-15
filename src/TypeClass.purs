module TypeClass where

import Prelude

class Glueable a where
  glue :: a -> a -> a

instance glueableString :: Glueable String where
  glue left right = left <> right

instance glueableInt :: Glueable Int where
  glue l r = l + r

instance glueableArrayChar :: Glueable (Array Char) where
  glue this that = this <> that

data Assistant
  = Siri
  | Alexa
  | Alisa
  | Google

instance showAssistant :: Show Assistant where
  show Siri = "Siri"
  show Alexa = "Alexa"
  show Alisa = "Alisa"
  show Google = "Goole"

class Polite x where
  greet :: x -> String
  goodbye :: x -> String

instance politeAssistant :: Polite Assistant where
  greet = case _ of
    Siri -> "Hello, what do u need?"
    Alexa -> "Hi my boi"
    Alisa -> "Привет"
    Google -> "Ok"
  goodbye ai = case ai of
    Siri -> "Bye, see u next time!"
    Alexa -> "Bye, see u next time!"
    Alisa -> "Bye, see u next time!"
    Google -> "Bye, see u next time!"

instance politeString :: Polite String where
  greet s = s <> " :)"
  goodbye str = str <> " :("

instance eqAssistant :: Eq Assistant where
  eq l r = show l == show r

instance assistantSemigroup :: Semigroup Assistant where
  append _ _ = Alexa

instance assistantMonoid :: Monoid Assistant where
  mempty = Siri
