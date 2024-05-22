module Json.Format
  ( Format (..)
  , coerce
  , float
  , int
  , bool
  , string
  , object2
  , object3
  , object4
  , requiredField
  , optionalField
  , title
  , description
  , defaultValue
  , examples
  )
where

import Data.Coerce (Coercible)
import Data.Coerce qualified
import Error qualified
import Float qualified
import Json (Json)
import Json qualified
import Json.FieldSchema (FieldSchema (FieldSchema))
import Json.FieldSchema qualified as FieldSchema
import Json.Schema (Schema)
import Json.Schema qualified
import {-# SOURCE #-} Json.Serialization (Serialization)
import {-# SOURCE #-} Json.Serialization qualified
import List qualified
import Map (Map)
import Map qualified
import OpenSolid
import Result qualified

data Format a = Format
  { encode :: a -> Json
  , decode :: Json -> Result String a
  , schema :: Schema
  }

title :: String -> Format a -> Format a
title value format =
  format{schema = (schema format){Json.Schema.title = Just value}}

description :: String -> Format a -> Format a
description value format =
  format{schema = (schema format){Json.Schema.description = Just value}}

defaultValue :: a -> Format a -> Format a
defaultValue value format =
  format{schema = (schema format){Json.Schema.default_ = Just (encode format value)}}

examples :: List a -> Format a -> Format a
examples values format =
  format{schema = (schema format){Json.Schema.examples = List.map (encode format) values}}

coerce :: Coercible a b => Format a -> Format b
coerce format = do
  let Format{encode, decode, schema} = format
  Format
    { encode = Data.Coerce.coerce >> encode
    , decode = decode >> Result.map Data.Coerce.coerce
    , schema =
        schema
          { Json.Schema.title = Nothing
          , Json.Schema.description = Nothing
          , Json.Schema.default_ = Nothing
          , Json.Schema.examples = []
          }
    }

bool :: Format Bool
bool =
  Format{encode = Json.bool, decode = decodeBool, schema = Json.Schema.boolean}
    |> title "Bool"
    |> description "A boolean true or false"

decodeBool :: Json -> Result String Bool
decodeBool (Json.Bool value) = Ok value
decodeBool _ = Error "Expected a boolean"

string :: Format String
string =
  Format{encode = Json.string, decode = decodeString, schema = Json.Schema.string}
    |> title "String"
    |> description "A string of text"

decodeString :: Json -> Result String String
decodeString (Json.String value) = Ok value
decodeString _ = Error "Expected a string"

float :: Format Float
float =
  Format{encode = Json.float, decode = decodeFloat, schema = Json.Schema.number}
    |> title "Float"
    |> description "A unitless floating-point number"

decodeFloat :: Json -> Result String Float
decodeFloat (Json.Number value) = Ok value
decodeFloat _ = Error "Expected a float"

int :: Format Int
int =
  Format{encode = Json.int, decode = decodeInt, schema = Json.Schema.integer}
    |> title "Int"
    |> description "An integer"

decodeInt :: Json -> Result String Int
decodeInt (Json.Number value) = Float.toInt value ?? Error "Expected an integer"
decodeInt _ = Error "Expected an integer"

encodeObject :: List (parent -> Map String Json -> Map String Json) -> parent -> Json
encodeObject writers parent = do
  let fields = List.foldl (\accumulated writer -> writer parent accumulated) Map.empty writers
  Json.map fields

decodeObject :: (Map String Json -> Result String a) -> Json -> Result String a
decodeObject fromFields = \case
  Json.Object fields -> fromFields fields
  _ -> Error "Expecting an object"

objectSchema :: List FieldSchema -> Schema
objectSchema fieldSchemas =
  Json.Schema.object
    { Json.Schema.required =
        fieldSchemas
          |> List.filter FieldSchema.required
          |> List.map FieldSchema.name
    , Json.Schema.properties =
        fieldSchemas
          |> List.map (\FieldSchema{name, schema} -> (name, schema))
          |> Map.fromList
    }

object2 :: (child1 -> child2 -> parent) -> Field parent child1 -> Field parent child2 -> Format parent
object2 constructor format1 format2 = do
  let (Field write1 read1 fieldSchema1) = format1
  let (Field write2 read2 fieldSchema2) = format2
  let encode = encodeObject [write1, write2]
  let decode = decodeObject $
        \fields -> Result.do
          value1 <- read1 fields
          value2 <- read2 fields
          Ok (constructor value1 value2)
  let schema = objectSchema [fieldSchema1, fieldSchema2]
  Format{encode, decode, schema}

object3 ::
  (child1 -> child2 -> child3 -> parent) ->
  Field parent child1 ->
  Field parent child2 ->
  Field parent child3 ->
  Format parent
object3 constructor format1 format2 format3 = do
  let (Field write1 read1 fieldSchema1) = format1
  let (Field write2 read2 fieldSchema2) = format2
  let (Field write3 read3 fieldSchema3) = format3
  let encode = encodeObject [write1, write2, write3]
  let decode = decodeObject $
        \fields -> Result.do
          value1 <- read1 fields
          value2 <- read2 fields
          value3 <- read3 fields
          Ok (constructor value1 value2 value3)
  let schema = objectSchema [fieldSchema1, fieldSchema2, fieldSchema3]
  Format{encode, decode, schema}

object4 ::
  (child1 -> child2 -> child3 -> child4 -> parent) ->
  Field parent child1 ->
  Field parent child2 ->
  Field parent child3 ->
  Field parent child4 ->
  Format parent
object4 constructor format1 format2 format3 format4 = do
  let (Field write1 read1 fieldSchema1) = format1
  let (Field write2 read2 fieldSchema2) = format2
  let (Field write3 read3 fieldSchema3) = format3
  let (Field write4 read4 fieldSchema4) = format4
  let encode = encodeObject [write1, write2, write3, write4]
  let decode = decodeObject $
        \fields -> Result.do
          value1 <- read1 fields
          value2 <- read2 fields
          value3 <- read3 fields
          value4 <- read4 fields
          Ok (constructor value1 value2 value3 value4)
  let schema = objectSchema [fieldSchema1, fieldSchema2, fieldSchema3, fieldSchema4]
  Format{encode, decode, schema}

data Field parent child = Field
  { write :: parent -> Map String Json -> Map String Json
  , read :: Map String Json -> Result String child
  , fieldSchema :: FieldSchema
  }

withinField :: String -> Result String a -> Result String a
withinField fieldName = Error.context ("In field \"" + fieldName + "\"")

requiredField :: Serialization child => String -> (parent -> child) -> Field parent child
requiredField fieldName getField = do
  let Format{encode = encodeField, decode = decodeField, schema} = Json.Serialization.format
  let write parent fields = Map.set fieldName (encodeField (getField parent)) fields
  let read fields = Result.do
        fieldJson <-
          Map.get fieldName fields
            ?? Error ("Expected a field named \"" + fieldName + "\"")
        withinField fieldName (decodeField fieldJson)
  let fieldSchema = FieldSchema{name = fieldName, required = True, schema}
  Field{write, read, fieldSchema}

optionalField ::
  Serialization child =>
  String ->
  (parent -> Maybe child) ->
  Field parent (Maybe child)
optionalField fieldName getField = do
  let Format{encode = encodeField, decode = decodeField, schema} = Json.Serialization.format
  let write parent fields =
        case getField parent of
          Just fieldValue -> Map.set fieldName (encodeField fieldValue) fields
          Nothing -> fields
  let read fields =
        case Map.get fieldName fields of
          Just Json.Null -> Ok Nothing
          Just fieldJson -> withinField fieldName (Result.map Just (decodeField fieldJson))
          Nothing -> Ok Nothing
  let fieldSchema = FieldSchema{name = fieldName, required = False, schema}
  Field{write, read, fieldSchema}
