module JSON where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, jsonEmptyString, (.!=), (.:), (.:?))
import Data.Argonaut as A
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Helper (notImplemented)

foreign import sampleJson1 :: Json

foreign import sampleArray10 :: Json

someNumber :: Json
someNumber = A.fromNumber 23.6

foreign import sampleHomoObject :: Object String

someObject :: Json
someObject =
  A.fromObject
    ( Object.fromFoldable
        [ Tuple "people"
            ( A.fromArray
                [ A.jsonSingletonObject "name" (A.fromString "John")
                , A.jsonSingletonObject "name" (A.fromString "Jane")
                ]
            )
        , Tuple "authors"
            ( A.fromArray
                [ A.jsonSingletonObject "name" (A.fromString "John")
                , A.jsonSingletonObject "name" (A.fromString "Jane")
                ]
            )
        ]
    )

someObject2 :: Either String Json
someObject2 =
  jsonParser
    """ 
      { "people": 
        [ { "name": "John" }
        , { "name": "Jane" }
        ] 
      } 
    """

prettyJson :: Json -> Effect Unit
prettyJson = A.stringifyWithIndent 2 >>> log

compactJson :: Json -> Effect Unit
compactJson = A.stringify >>> log

jsonType :: Json -> String
jsonType =
  A.caseJson
    (const "null")
    (const "bool")
    (const "number")
    (const "string")
    (const "array")
    (const "object")

-- Counts scalar json values recursively,
-- e.g. [1, true, { "name": "ok" }] = 3
-- countScalars sampleJson1 = 12
countScalars :: Json -> Int
countScalars =
  A.caseJson
    (const 1)
    (const 1)
    (const 1)
    (const 1)
    (\a -> countArray a)
    (\o -> countArray (Object.values o))
  where
  countArray = Array.foldr (\el a -> a + countScalars el) 0

-- (\o -> countArray (Object.values o))
-- | ```purescript
-- | compactJson (nest 0) == []
-- | compactJson (nest 1) == [[]]
-- | compactJson (nest 2) == [[[]]]
-- | compactJson (nest 3) == [[[[]]]]
-- | ```
nest :: Int -> Json
nest iter = (go { iter, json: A.jsonEmptyArray }).json
  where
  go a
    | a.iter < 1 = a
    | otherwise = go { iter: a.iter - 1, json: A.jsonSingletonArray a.json }

data Command
  = Eat Int
  | Sleep

showCommand :: Command -> String
showCommand = case _ of
  Eat n -> "Eat " <> show n
  Sleep -> "Sleep"

instance encodeJsonCommand :: EncodeJson Command where
  encodeJson :: Command -> Json
  encodeJson = case _ of
    Eat n ->
      "tag" := "eat"
        ~> "amount"
        := n
        ~> jsonEmptyObject
    Sleep ->
      "tag" := "sleep"
        ~> jsonEmptyObject

type Potential x
  = Either JsonDecodeError x

instance decodeJsonCommand :: DecodeJson Command where
  decodeJson json = decodeSleepCons json <|> decodeEatCons json

decodeEatCons :: Json -> Either JsonDecodeError Command
decodeEatCons json = do
  { tag, amount } :: { tag :: String, amount :: Int } <- decodeJson json
  case tag of
    "eat" -> Right (Eat amount)
    _ -> Left (UnexpectedValue json)

decodeSleepCons :: Json -> Either JsonDecodeError Command
decodeSleepCons json = do
  { tag } :: { tag :: String } <- decodeJson json
  case tag of
    "sleep" -> Right Sleep
    _ -> Left (UnexpectedValue json)

data Drill
  = PushingHands
  | Sparring

derive instance eqDrill :: Eq Drill

derive instance ordDrill :: Ord Drill

instance showDrill :: Show Drill where
  show = case _ of
    PushingHands -> "Pushing Hands"
    Sparring -> "Sparring"

data Kungfu
  = Kungfu
    { chineseName :: String
    , qiPowerScore :: Int
    , drills :: Set Drill
    , master :: Maybe String
    }

wingChun :: Kungfu
wingChun =
  Kungfu
    { chineseName: "功夫"
    , qiPowerScore: 5
    , drills: Set.fromFoldable [ PushingHands, Sparring ]
    , master: Just "Ip Man"
    }


instance encodeJsonDrill :: EncodeJson Drill where
  encodeJson :: Drill -> Json
  encodeJson = case _ of
    PushingHands ->
      encodeString "PushingHands"
    Sparring ->
      encodeString "Sparring"

decodePushingHands :: Json -> Either JsonDecodeError Drill
decodePushingHands json = do
  value :: String  <- decodeString json
  case value of
    "PushingHands" -> Right PushingHands
    _ -> Left (UnexpectedValue json)

decodeSparring :: Json -> Either JsonDecodeError Drill
decodeSparring json = do
  value :: String  <- decodeString json
  case value of
    "Sparring" -> Right Sparring
    _ -> Left (UnexpectedValue json)

instance decodeJsonDrill :: DecodeJson Drill where
  decodeJson json = decodePushingHands json <|> decodeSparring json

instance encodeJsonKungfu :: EncodeJson Kungfu where
  encodeJson (Kungfu obj) =
    "chineseName" := obj.chineseName
    ~> "qiPowerScore" := obj.qiPowerScore
    ~> "drills" := obj.drills
    ~> "master" := obj.master
    ~> jsonEmptyObject


instance decodeJsonKungfu :: DecodeJson Kungfu where
  decodeJson json = do
    obj <- decodeJson json
    chineseName <- obj .: "chineseName"
    qiPowerScore <- obj .: "qiPowerScore"
    drills <- obj .: "drills"
    master <- obj .:? "master"
    pure $ Kungfu { chineseName: chineseName, qiPowerScore: qiPowerScore, drills: drills, master: master}

decodeKungfu :: Json -> Either JsonDecodeError Kungfu
decodeKungfu = decodeJson

roundtripKungfu :: Kungfu -> Boolean
roundtripKungfu =
  encodeJson
    >>> decodeKungfu
    >>> either (const false) (const true)

-- Homework: make `roundtripKungfu wingChun` return true!
