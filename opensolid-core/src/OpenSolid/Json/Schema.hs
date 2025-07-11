module OpenSolid.Json.Schema
  ( Schema (title, description, default_, examples, type_, required, properties, items, minItems)
  , integer
  , number
  , boolean
  , string
  , object
  , array
  )
where

import OpenSolid.Json (Json)
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Prelude

data Schema = Schema
  { title :: Maybe Text
  , description :: Maybe Text
  , default_ :: Maybe Json
  , examples :: List Json
  , type_ :: Text
  , required :: List Text
  , properties :: Map Text Schema
  , items :: Maybe Schema
  , minItems :: Maybe Int
  }
  deriving (Show)

hasType :: Text -> Schema
hasType t =
  Schema
    { title = Nothing
    , description = Nothing
    , default_ = Nothing
    , examples = []
    , type_ = t
    , required = []
    , properties = Map.empty
    , items = Nothing
    , minItems = Nothing
    }

integer :: Schema
integer = hasType "integer"

number :: Schema
number = hasType "number"

boolean :: Schema
boolean = hasType "boolean"

string :: Schema
string = hasType "string"

object :: Schema
object = hasType "object"

array :: Schema
array = hasType "array"
