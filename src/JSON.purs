module JSON where

import Prelude
import Data.Argonaut (Json)
import Data.Argonaut as A
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object

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
