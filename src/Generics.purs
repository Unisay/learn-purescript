-- | Write Your own Generics: https://harry.garrood.me/blog/write-your-own-generics/
module Generics where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (pow)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))

data GSum a b
  = GLeft a
  | GRight b

derive instance genericGSum :: Generic (GSum a b) _

instance showGSum :: (Show a, Show b) => Show (GSum a b) where
  show = genericShow

data GProduct a b
  = GBoth a b

instance showGProduct :: (Show a, Show b) => Show (GProduct a b) where
  show (GBoth a b) = "GBoth " <> show a <> " " <> show b

infixl 6 type GSum as :+:

infixl 7 type GProduct as :*:

infixl 7 GBoth as :*:

type MaybeRep a = Unit :+: a

repFromMaybe :: forall a. Maybe a -> MaybeRep a
repFromMaybe = case _ of
  Nothing -> GLeft unit
  Just x -> GRight x

repToMaybe :: forall a. MaybeRep a -> Maybe a
repToMaybe = case _ of
  GLeft _ -> Nothing
  GRight x -> Just x

maybeIsomorphismProof :: forall a. Eq a => a -> Boolean
maybeIsomorphismProof a = proofFor Nothing && proofFor (Just a)
  where proofFor ma = repToMaybe (repFromMaybe ma) == ma

type TupleRep a b = a :*: b

repFromTuple :: forall a b. Tuple a b -> TupleRep a b
repFromTuple (Tuple a b) = GBoth a b

repToTuple :: forall a b. TupleRep a b -> Tuple a b
repToTuple (a :*: b) = Tuple a b

tupleIsomorphismProof :: forall a b. Eq a => Eq b => a -> b -> Boolean
tupleIsomorphismProof a b = repToTuple (repFromTuple t) == t
  where t = Tuple a b

maybeTupleIsomorphismProof :: forall a b. Eq a => Eq b => a -> b -> Boolean
maybeTupleIsomorphismProof a b = 
 proofFor Nothing && proofFor (Just (Tuple a b))
 where 
 proofFor ma = repToMaybe (repFromMaybe ma) == ma

--------------------- 
--- Covid Strains --- 
--------------------- 

data CovidStrain = Alpha | Beta | Gamma | Delta

derive instance Eq CovidStrain

type CovidStrainRep = GSum (GSum (GSum Unit Unit) Unit) Unit

repFromCovidStrain :: CovidStrain -> CovidStrainRep
repFromCovidStrain = case _ of
  Alpha -> GLeft (GLeft (GLeft unit)) 
  Beta -> GLeft (GLeft (GRight unit)) 
  Gamma -> GLeft (GRight unit) 
  Delta -> GRight unit 

repToCovidStrain :: CovidStrainRep -> CovidStrain
repToCovidStrain = case _ of
  GLeft (GLeft (GLeft _)) -> Alpha
  GLeft (GLeft (GRight _)) -> Beta
  GLeft (GRight _) -> Gamma
  GRight _ -> Delta

------------------------------------
--- Generic Representation Class ---
------------------------------------

class GenericRep t r | t -> r where
  to :: t -> r
  fr :: r -> t

instance genericCovidStrain :: GenericRep CovidStrain CovidStrainRep where
  to :: CovidStrain -> CovidStrainRep
  to = repFromCovidStrain

  fr :: CovidStrainRep -> CovidStrain
  fr = repToCovidStrain

instance GenericRep (Maybe a) (MaybeRep a) where
  to = repFromMaybe
  fr = repToMaybe

instance GenericRep (Tuple a b) (GProduct a b) where
  to = repFromTuple
  fr = repToTuple

instance GenericRep (Either a b) (GSum a b) where
  to = case _ of
    Left a -> GLeft a
    Right b -> GRight b
  fr = case _ of
    GLeft a -> Left a
    GRight a -> Right a

----------------------
--- Funny Encoding ---
----------------------

class FunnyEncode a where
  encodeFunny :: a -> String

genericEncodeFunny :: 
  forall t r .  
  GenericRep t r => 
  FunnyEncode r  => 
  t -> String
genericEncodeFunny = encodeFunny <<< to

instance FunnyEncode Unit where encodeFunny _ = "."
instance FunnyEncode Int where encodeFunny i = show i
instance FunnyEncode String where encodeFunny s = s
instance FunnyEncode Char where encodeFunny c = String.singleton c
instance FunnyEncode a => FunnyEncode (Maybe a) 
  where encodeFunny = genericEncodeFunny

instance encodeFunnyGSum :: 
  (FunnyEncode a,  FunnyEncode b) => FunnyEncode (GSum a b) where
  encodeFunny = case _ of
    GLeft a  -> "(" <> encodeFunny a <> "|)" 
    GRight b  -> "(|" <> encodeFunny b <> ")" 

instance encodeFunnyGProduct :: 
  (FunnyEncode a,  FunnyEncode b) => FunnyEncode (GProduct a b) where
  encodeFunny (GBoth a b) = "(" <> encodeFunny a <> "<|>" <> encodeFunny b <> ")" 

