module Json
  ( Json (.., Json.Int, Object)
  , Format
  , Schema
  , object
  , text
  , int
  , float
  , bool
  , list
  , map
  , encode
  , decode
  )
where

import Composition
import Data.Aeson qualified
import Data.Aeson.KeyMap qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.Scientific
import Data.Vector qualified
import Float qualified
import {-# SOURCE #-} Json.Format (Format)
import {-# SOURCE #-} Json.Schema (Schema)
import List qualified
import Map (Map)
import Map qualified
import OpenSolid hiding (pattern Int)
import Text qualified
import Prelude qualified

data Json
  = Null
  | Bool Bool
  | Float Float
  | Text Text
  | List (List Json)
  | Map (Map Text Json)
  deriving (Eq, Show)

pattern Int :: Int -> Json
pattern Int n <- Float (Float.Int n)

pattern Object :: List (Text, Json) -> Json
pattern Object fields <- (getFields -> Just fields)

getFields :: Json -> Maybe (List (Text, Json))
getFields (Map fields) = Just (Map.toList fields)
getFields _ = Nothing

int :: Int -> Json
int = Float.int >> Float

float :: Float -> Json
float = Float

bool :: Bool -> Json
bool = Bool

text :: Text -> Json
text = Text

list :: (a -> Json) -> List a -> Json
list encodeItem items = List (List.map encodeItem items)

object :: List (Text, Json) -> Json
object fields = Map (Map.fromList fields)

map :: Map Text Json -> Json
map = Map

instance Data.Aeson.ToJSON Json where
  toJSON json = case json of
    Null -> Data.Aeson.Null
    Bool value -> Data.Aeson.toJSON value
    Float value -> Data.Aeson.toJSON (Float.toDouble value)
    Text value -> Data.Aeson.toJSON value
    List values -> Data.Aeson.toJSON values
    Map fields -> Data.Aeson.toJSON fields

instance Data.Aeson.FromJSON Json where
  parseJSON = fromAeson >> Prelude.return

fromAeson :: Data.Aeson.Value -> Json
fromAeson aesonValue = case aesonValue of
  Data.Aeson.Null -> Null
  Data.Aeson.Bool value -> Bool value
  Data.Aeson.Number value -> Float (Data.Scientific.toRealFloat value)
  Data.Aeson.String value -> Text value
  Data.Aeson.Array values -> List (List.map fromAeson (Data.Vector.toList values))
  Data.Aeson.Object values -> Map (Map.map fromAeson (Data.Aeson.KeyMap.toMapText values))

encode :: Json -> ByteString
encode = Data.Aeson.encode >> Data.ByteString.toStrict

decode :: ByteString -> Result Text Json
decode byteString =
  case Data.Aeson.eitherDecodeStrict byteString of
    Prelude.Right json -> Ok json
    Prelude.Left error -> Error (Text.pack error)
