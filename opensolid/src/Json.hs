module Json
  ( Json (..)
  , Format
  , Schema
  , object
  , string
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
import Data.ByteString.Lazy (ByteString)
import Data.Scientific
import Data.Vector qualified
import Float qualified
import {-# SOURCE #-} Json.Format (Format)
import {-# SOURCE #-} Json.Schema (Schema)
import List qualified
import Map (Map)
import Map qualified
import OpenSolid
import Text qualified
import Prelude qualified

data Json
  = Null
  | Bool Bool
  | Number Float
  | String Text
  | Array (List Json)
  | Object (Map Text Json)
  deriving (Eq, Show)

int :: Int -> Json
int = Float.fromInt >> Number

float :: Float -> Json
float = Number

bool :: Bool -> Json
bool = Bool

string :: Text -> Json
string = String

list :: (a -> Json) -> List a -> Json
list encodeItem items = Array (List.map encodeItem items)

object :: List (Text, Json) -> Json
object fields = Object (Map.fromList fields)

map :: Map Text Json -> Json
map = Object

instance Data.Aeson.ToJSON Json where
  toJSON = \case
    Null -> Data.Aeson.Null
    Bool value -> Data.Aeson.toJSON value
    Number value -> Data.Aeson.toJSON (Float.toDouble value)
    String value -> Data.Aeson.toJSON value
    Array values -> Data.Aeson.toJSON values
    Object fields -> Data.Aeson.toJSON fields

instance Data.Aeson.FromJSON Json where
  parseJSON = fromAeson >> Prelude.return

fromAeson :: Data.Aeson.Value -> Json
fromAeson = \case
  Data.Aeson.Null -> Null
  Data.Aeson.Bool value -> Bool value
  Data.Aeson.Number value -> Number (Data.Scientific.toRealFloat value)
  Data.Aeson.String value -> String value
  Data.Aeson.Array values -> Array (List.map fromAeson (Data.Vector.toList values))
  Data.Aeson.Object values -> Object (Map.map fromAeson (Data.Aeson.KeyMap.toMapText values))

encode :: Json -> ByteString
encode = Data.Aeson.encode

decode :: ByteString -> Result Text Json
decode byteString =
  case Data.Aeson.eitherDecode byteString of
    Prelude.Right json -> Ok json
    Prelude.Left error -> Error (Text.pack error)
