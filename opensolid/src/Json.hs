module Json
  ( Json (..)
  , Format
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
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.ByteString.Lazy (ByteString)
import Data.Scientific
import Data.Text qualified
import Data.Vector qualified
import Float qualified
import List qualified
import {-# SOURCE #-} Json.Format (Format)
import Map (Map)
import Map qualified
import OpenSolid
import Prelude qualified

data Json
  = Null
  | Bool Bool
  | Number Float
  | String String
  | Array (List Json)
  | Object (Map String Json)
  deriving (Eq, Show)

int :: Int -> Json
int = Float.fromInt >> Number

float :: Float -> Json
float = Number

bool :: Bool -> Json
bool = Bool

string :: String -> Json
string = String

list :: (a -> Json) -> List a -> Json
list encodeItem items = Array (List.map encodeItem items)

object :: List (String, Json) -> Json
object fields = Object (Map.fromList fields)

map :: Map String Json -> Json
map = Object

toAesonValue :: Json -> Data.Aeson.Value
toAesonValue = \case
  Null -> Data.Aeson.Null
  Bool value -> Data.Aeson.toJSON value
  Number value -> Data.Aeson.toJSON (Float.toDouble value)
  String value -> Data.Aeson.toJSON (Data.Text.pack value)
  Array values -> Data.Aeson.toJSON (List.map toAesonValue values)
  Object fields -> do
    let keyMapEntry (fieldName, fieldValue) =
          (Data.Aeson.Key.fromString fieldName, toAesonValue fieldValue)
    let entries = List.map keyMapEntry (Map.toList fields)
    Data.Aeson.toJSON (Data.Aeson.KeyMap.fromList entries)

fromAesonValue :: Data.Aeson.Value -> Json
fromAesonValue = \case
  Data.Aeson.Null -> Null
  Data.Aeson.Bool value -> Bool value
  Data.Aeson.Number value -> Number (Data.Scientific.toRealFloat value)
  Data.Aeson.String value -> String (Data.Text.unpack value)
  Data.Aeson.Array values ->
    Array (values |> Data.Vector.map fromAesonValue |> Data.Vector.toList)
  Data.Aeson.Object values -> do
    let mapEntry (key, value) = (Data.Aeson.Key.toString key, fromAesonValue value)
    Object (values |> Data.Aeson.KeyMap.toList |> List.map mapEntry |> Map.fromList)

encode :: Json -> ByteString
encode = toAesonValue >> Data.Aeson.encode

decode :: ByteString -> Result String Json
decode byteString =
  case Data.Aeson.eitherDecode byteString of
    Prelude.Right aesonValue -> Ok (fromAesonValue aesonValue)
    Prelude.Left error -> Error error
