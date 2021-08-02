module JSON where

import Prelude
import Data.Argonaut (Json)
import Data.Argonaut as A
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object as Object
import Helper (notImplemented)

foreign import sampleJson1 :: Json

someNumber :: Json
someNumber = A.fromNumber 23.6

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
--  e.g. [1, true, { "name": "ok" }] = 3
-- countScalars sampleJson1 = 12
countScalars :: Json -> Int
countScalars = notImplemented
