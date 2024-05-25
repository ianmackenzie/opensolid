module Json.Format
  ( Format (..)
  , coerce
  , map
  , validate
  , float
  , int
  , bool
  , text
  , list
  , nonEmpty
  , object
  , singleField
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
import List qualified
import Map (Map)
import Map qualified
import NonEmpty qualified
import OpenSolid
import Result qualified

data Format a = Format
  { encode :: a -> Json
  , decode :: Json -> Result Text a
  , schema :: Schema
  }

title :: Text -> Format a -> Format a
title value format =
  format{schema = (schema format){Json.Schema.title = Just value}}

description :: Text -> Format a -> Format a
description value format =
  format{schema = (schema format){Json.Schema.description = Just value}}

defaultValue :: a -> Format a -> Format a
defaultValue value format =
  format{schema = (schema format){Json.Schema.default_ = Just (encode format value)}}

examples :: List a -> Format a -> Format a
examples values format =
  format{schema = (schema format){Json.Schema.examples = List.map (encode format) values}}

removeMetadata :: Schema -> Schema
removeMetadata schema =
  schema
    { Json.Schema.title = Nothing
    , Json.Schema.description = Nothing
    , Json.Schema.default_ = Nothing
    , Json.Schema.examples = []
    }

coerce :: Coercible a b => Format a -> Format b
coerce = map Data.Coerce.coerce Data.Coerce.coerce

map :: (a -> b) -> (b -> a) -> Format a -> Format b
map lift drop format = validate (lift >> Ok) drop format

validate :: (a -> Result x b) -> (b -> a) -> Format a -> Format b
validate lift drop format = do
  let Format{encode, decode, schema} = format
  Format
    { encode = drop >> encode
    , decode = decode >> Result.andThen (lift >> Result.mapError Error.message)
    , schema = removeMetadata schema
    }

bool :: Format Bool
bool =
  Format{encode = Json.bool, decode = decodeBool, schema = Json.Schema.boolean}
    |> title "Bool"
    |> description "A boolean true or false"

decodeBool :: Json -> Result Text Bool
decodeBool (Json.Bool value) = Ok value
decodeBool _ = Error "Expected a boolean"

text :: Format Text
text =
  Format{encode = Json.string, decode = decodeText, schema = Json.Schema.string}
    |> title "Text"
    |> description "Arbitrary text"

decodeText :: Json -> Result Text Text
decodeText (Json.String value) = Ok value
decodeText _ = Error "Expected a string"

float :: Format Float
float =
  Format{encode = Json.float, decode = decodeFloat, schema = Json.Schema.number}
    |> title "Float"
    |> description "A unitless floating-point number"

decodeFloat :: Json -> Result Text Float
decodeFloat (Json.Number value) = Ok value
decodeFloat _ = Error "Expected a float"

int :: Format Int
int =
  Format{encode = Json.int, decode = decodeInt, schema = Json.Schema.integer}
    |> title "Int"
    |> description "An integer"

decodeInt :: Json -> Result Text Int
decodeInt (Json.Number value) = Float.toInt value ?? Error "Expected an integer"
decodeInt _ = Error "Expected an integer"

decodeArray :: (Json -> Result Text a) -> Json -> Result Text (List a)
decodeArray decodeItem = \case
  Json.Array items -> Result.collect decodeItem items
  _ -> Error "Expected an array"

list :: Format item -> Format (List item)
list (Format encodeItem decodeItem itemSchema) =
  Format
    { encode = Json.list encodeItem
    , decode = decodeArray decodeItem
    , schema = Json.Schema.array{Json.Schema.items = Just itemSchema}
    }

toNonEmpty :: List item -> Result Text (NonEmpty item)
toNonEmpty (NonEmpty items) = Ok items
toNonEmpty [] = Error "Array is empty"

nonEmpty :: Format item -> Format (NonEmpty item)
nonEmpty (Format encodeItem decodeItem itemSchema) =
  Format
    { encode = NonEmpty.toList >> Json.list encodeItem
    , decode = decodeArray decodeItem >> Result.andThen toNonEmpty
    , schema = Json.Schema.array{Json.Schema.items = Just itemSchema, Json.Schema.minItems = Just 1}
    }

decodeObject :: (Map Text Json -> Result Text a) -> Json -> Result Text a
decodeObject fromFields = \case
  Json.Object fields -> fromFields fields
  _ -> Error "Expected an object"

object :: constructor -> Fields constructor parent () -> Format parent
object constructor (Fields decompose compose properties required) =
  Format
    { encode = decompose >> Json.map
    , decode = decodeObject (\fieldValues -> compose fieldValues constructor)
    , schema = Json.Schema.object{Json.Schema.required, Json.Schema.properties}
    }

singleField :: (child -> parent) -> Field parent child () -> Format parent
singleField constructor field = object constructor (lastField field)

data Field parent child dummy = Field
  { write :: parent -> Map Text Json -> Map Text Json
  , read :: Map Text Json -> Result Text child
  , fieldSchema :: FieldSchema
  }

data Fields constructor parent dummy = Fields
  { decompose :: parent -> Map Text Json
  , compose :: Map Text Json -> constructor -> Result Text parent
  , properties :: Map Text Schema
  , required :: List Text
  }

instance
  parent ~ parent' =>
  Composition
    (Field parent child ())
    (Fields constructor parent' ())
    (Fields (child -> constructor) parent ())
  where
  field >> fields = do
    let (Field write read fieldSchema) = field
    let (Fields decompose compose properties required) = fields
    Fields
      { decompose = \parent -> decompose parent |> write parent
      , compose = \fieldValues constructor ->
          read fieldValues
            |> Result.map constructor
            |> Result.andThen (compose fieldValues)
      , properties = properties |> Map.set (FieldSchema.name fieldSchema) (FieldSchema.schema fieldSchema)
      , required = [FieldSchema.name fieldSchema | FieldSchema.required fieldSchema] + required
      }

instance
  parent ~ parent' =>
  Composition
    (Field parent child1 ())
    (Field parent' child2 ())
    (Fields (child1 -> child2 -> parent) parent ())
  where
  field1 >> field2 = field1 >> lastField field2

lastField :: Field parent child () -> Fields (child -> parent) parent ()
lastField (Field write read fieldSchema) = do
  let decompose parent = write parent Map.empty
  let compose fields constructor = Result.map constructor (read fields)
  let properties = Map.singleton (FieldSchema.name fieldSchema) (FieldSchema.schema fieldSchema)
  let required = [FieldSchema.name fieldSchema | FieldSchema.required fieldSchema]
  Fields{decompose, compose, properties, required}

withinField :: Text -> Result Text a -> Result Text a
withinField fieldName = Error.context ("In field \"" + fieldName + "\"")

requiredField :: Text -> (parent -> child) -> Format child -> Field parent child ()
requiredField fieldName getField fieldFormat = do
  let Format{encode = encodeField, decode = decodeField, schema} = fieldFormat
  let write parent fields = Map.set fieldName (encodeField (getField parent)) fields
  let read fields = Result.do
        fieldJson <-
          Map.get fieldName fields
            ?? Error ("Expected a field named \"" + fieldName + "\"")
        withinField fieldName (decodeField fieldJson)
  let fieldSchema = FieldSchema{name = fieldName, required = True, schema}
  Field{write, read, fieldSchema}

optionalField ::
  Text ->
  (parent -> Maybe child) ->
  Format child ->
  Field parent (Maybe child) ()
optionalField fieldName getField fieldFormat = do
  let Format{encode = encodeField, decode = decodeField, schema} = fieldFormat
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
