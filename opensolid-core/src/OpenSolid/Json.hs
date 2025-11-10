module OpenSolid.Json
  ( Json (.., OpenSolid.Json.Int, Object)
  , Format
  , Schema
  , Field
  , field
  , optional
  , object
  , text
  , int
  , number
  , bool
  , list
  , listOf
  , map
  , toBinary
  , decode
  )
where

import Data.Aeson qualified
import Data.Aeson.KeyMap qualified
import Data.Scientific
import Data.Vector qualified
import OpenSolid.Binary (Builder, ByteString)
import {-# SOURCE #-} OpenSolid.Json.Format (Format)
import {-# SOURCE #-} OpenSolid.Json.Schema (Schema)
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Prelude qualified

data Json
  = Null
  | Bool Bool
  | Number Number
  | Text Text
  | List (List Json)
  | Map (Map Text Json)
  deriving (Eq, Show)

newtype Field = Field (Maybe (Text, Json))

pattern Int :: Int -> Json
pattern Int n <- (toInt -> Just n)

toInt :: Json -> Maybe Int
toInt (Number value) = do
  let rounded = Number.round value
  if Number.fromInt rounded == value then Just rounded else Nothing
toInt _ = Nothing

pattern Object :: List (Text, Json) -> Json
pattern Object fields <- (getFields -> Just fields)

getFields :: Json -> Maybe (List (Text, Json))
getFields (Map fields) = Just (Map.toList fields)
getFields _ = Nothing

int :: Int -> Json
int = Number . Number.fromInt

number :: Number -> Json
number = Number

bool :: Bool -> Json
bool = Bool

text :: Text -> Json
text = Text

list :: List Json -> Json
list = List

listOf :: (a -> Json) -> List a -> Json
listOf encodeItem items = List (List.map encodeItem items)

field :: Text -> Json -> Field
field name value = Field (Just (name, value))

optional :: Text -> Maybe Json -> Field
optional name maybeValue = Field (Maybe.map (name,) maybeValue)

keyValuePair :: Field -> Maybe (Text, Json)
keyValuePair (Field kv) = kv

object :: List Field -> Json
object fields = Map (Map.fromKeyValuePairs (List.filterMap keyValuePair fields))

map :: Map Text Json -> Json
map = Map

instance Data.Aeson.ToJSON Json where
  toJSON json = case json of
    Null -> Data.Aeson.Null
    Bool value -> Data.Aeson.toJSON value
    Number value -> Data.Aeson.toJSON (Number.toDouble value)
    Text value -> Data.Aeson.toJSON value
    List values -> Data.Aeson.toJSON values
    Map fields -> Data.Aeson.toJSON fields

instance Data.Aeson.FromJSON Json where
  parseJSON = Prelude.return . fromAeson

fromAeson :: Data.Aeson.Value -> Json
fromAeson aesonValue = case aesonValue of
  Data.Aeson.Null -> Null
  Data.Aeson.Bool value -> Bool value
  Data.Aeson.Number value -> Number (Data.Scientific.toRealFloat value)
  Data.Aeson.String value -> Text value
  Data.Aeson.Array values -> List (List.map fromAeson (Data.Vector.toList values))
  Data.Aeson.Object values -> Map (Map.map fromAeson (Data.Aeson.KeyMap.toMapText values))

toBinary :: Json -> Builder
toBinary json = Data.Aeson.fromEncoding (Data.Aeson.toEncoding json)

decode :: ByteString -> Result Text Json
decode byteString =
  case Data.Aeson.eitherDecodeStrict byteString of
    Right json -> Success json
    Left error -> Failure (Text.pack error)
