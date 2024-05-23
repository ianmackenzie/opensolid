module Json.Schema
  ( Schema (..)
  , integer
  , number
  , boolean
  , string
  , object
  , array
  )
where

import Json (Json)
import Map (Map)
import Map qualified
import OpenSolid

data Schema = Schema
  { title :: Maybe String
  , description :: Maybe String
  , default_ :: Maybe Json
  , examples :: List Json
  , type_ :: String
  , required :: List String
  , properties :: Map String Schema
  , minItems :: Maybe Int
  }
  deriving (Show)

hasType :: String -> Schema
hasType t =
  Schema
    { title = Nothing
    , description = Nothing
    , default_ = Nothing
    , examples = []
    , type_ = t
    , required = []
    , properties = Map.empty
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
