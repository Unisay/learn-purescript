module Class.MyNewtype where

import Prelude

import Data.String as String
import Homework.Todo (todo)

type SoCool = Int

newtype Cool = Cool Int

instance showCool :: Show Cool where
  show (Cool i) = "Cool (" <> show i <> ")"

newtype Foo a = MkFoo a

instance showFoo :: Show a => Show (Foo a) where
  show f = "Foo (" <> show (unwrap f) <> ")"

newtype Bar b = Bar b

instance showBar :: Show a => Show (Bar a) where
  show (Bar i) = "Bar (" <> show i <> ")"

newtype Baz (a :: Type -> Type) b = Baz (a b)

instance showBaz :: (Show (a b), Show b, MyNewtype a b) => Show (Baz a b) where
  show (Baz ab) = "Baz (" <> show ab <> ")"

newtype Phantom :: forall kb kc kd ke. Type -> kb -> kc -> kd -> ke -> Type
newtype Phantom a b c d e = Phantom a

class MyNewtype n a | n -> a where
  wrap :: a -> n a
  unwrap :: n a -> a

-- | Given a constructor for a `Newtype`, this returns the appropriate `unwrap`
-- | function.
-- | > un Phantom (wrap 42)
-- | 42
un :: forall t a. MyNewtype t a => (a -> t a) -> t a -> a
un _ = unwrap

instance myNewTypeFoo :: MyNewtype Foo a where
  wrap :: a -> Foo a
  wrap = MkFoo
  unwrap :: Foo a -> a
  unwrap (MkFoo a) = a

instance myNewTypeBari :: MyNewtype Bar a where
  wrap = Bar
  unwrap (Bar a) = a

instance myNewTypeBaz :: MyNewtype a b => MyNewtype (Baz a) b where
  wrap :: b -> (Baz a) b
  wrap b = Baz ((wrap b) :: a b)
  unwrap :: (Baz a) b -> b
  unwrap (Baz (ab :: a b)) = unwrap ab

instance myNewTypePhantom :: MyNewtype (Phantom a b c d) a where
  wrap :: forall e. a -> Phantom a b c d e
  wrap = Phantom
  unwrap :: forall e. Phantom a b c d e -> a
  unwrap (Phantom x) = x

over :: forall t a s b. MyNewtype t a => MyNewtype s b => (a -> b) -> t a -> s b
over _ = todo "Homework"

-- | In repl:
-- |
-- | ```purescript
-- | > toUpperFoo (Bar "ok")
-- | Foo ("OK")
-- | ```
toUpperFoo :: Bar String -> Foo String
toUpperFoo = over String.toUpper

-- | In repl:
-- |
-- | ```purescript
-- | > toUpperFoo (MkFoo "ok")
-- | Foo ("OK")
-- | ```
toUpperFoo' :: Foo String -> Foo String
toUpperFoo' = over String.toUpper
