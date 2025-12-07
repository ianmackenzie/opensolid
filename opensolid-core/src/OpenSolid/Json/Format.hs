module OpenSolid.Json.Format
  ( Format (schema)
  , encode
  , decode
  , coerce
  , convert
  , lift
  , number
  , int
  , bool
  , text
  , list
  , nonEmpty
  , map
  , object
  , singleField
  , requiredField
  , optionalField
  , title
  , description
  , defaultValue
  , examples
  , angle
  , length
  , direction2d
  , vector2d
  , point2d
  )
where

import Data.Coerce (Coercible)
import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Json.FieldSchema (FieldSchema (FieldSchema))
import OpenSolid.Json.FieldSchema qualified as FieldSchema
import OpenSolid.Json.Schema qualified as Json.Schema
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D, pattern Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polymorphic.Vector2d (Vector2d (Vector2d))
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude hiding (compose, (>>))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2D (Vector2D, pattern Vector2D)
import OpenSolid.Vector2D qualified as Vector2D

data Format a = Format
  { encodeFunction :: a -> Json
  , decodeFunction :: Json -> Result Text a
  , schema :: Json.Schema
  }

encode :: Format a -> a -> Json
encode format = format.encodeFunction

decode :: Format a -> Json -> Result Text a
decode format = format.decodeFunction

title :: Text -> Format a -> Format a
title value format =
  format{schema = format.schema{Json.Schema.title = Just value}}

description :: Text -> Format a -> Format a
description value format =
  format{schema = format.schema{Json.Schema.description = Just value}}

defaultValue :: a -> Format a -> Format a
defaultValue value format =
  format{schema = format.schema{Json.Schema.default_ = Just (encode format value)}}

examples :: List a -> Format a -> Format a
examples values format =
  format{schema = format.schema{Json.Schema.examples = List.map (encode format) values}}

removeMetadata :: Json.Schema -> Json.Schema
removeMetadata schema =
  schema
    { Json.Schema.title = Nothing
    , Json.Schema.description = Nothing
    , Json.Schema.default_ = Nothing
    , Json.Schema.examples = []
    }

coerce :: Coercible a b => Format a -> Format b
coerce = convert Data.Coerce.coerce Data.Coerce.coerce

convert :: (a -> b) -> (b -> a) -> Format a -> Format b
convert up down format = lift (Ok . up) down format

lift :: (a -> Result x b) -> (b -> a) -> Format a -> Format b
lift up down format = do
  let Format{encodeFunction, decodeFunction, schema} = format
  Format
    { encodeFunction = encodeFunction . down
    , decodeFunction = \json -> do
        unlifted <- decodeFunction json
        Result.orFail (up unlifted)
    , schema = removeMetadata schema
    }

bool :: Format Bool
bool =
  Format{encodeFunction = Json.bool, decodeFunction = decodeBool, schema = Json.Schema.boolean}
    & title "Bool"
    & description "A boolean true or false"

decodeBool :: Json -> Result Text Bool
decodeBool (Json.Bool value) = Ok value
decodeBool _ = Error "Expected a boolean"

text :: Format Text
text =
  Format{encodeFunction = Json.text, decodeFunction = decodeText, schema = Json.Schema.string}
    & title "Text"
    & description "Arbitrary text"

decodeText :: Json -> Result Text Text
decodeText (Json.Text value) = Ok value
decodeText _ = Error "Expected text"

number :: Format Number
number =
  Format{encodeFunction = Json.number, decodeFunction = decodeNumber, schema = Json.Schema.number}
    & title "Number"
    & description "A unitless floating-point number"

decodeNumber :: Json -> Result Text Number
decodeNumber (Json.Number value) = Ok value
decodeNumber _ = Error "Expected a number"

int :: Format Int
int =
  Format{encodeFunction = Json.int, decodeFunction = decodeInt, schema = Json.Schema.integer}
    & title "Int"
    & description "An integer"

decodeInt :: Json -> Result Text Int
decodeInt (Json.Int value) = Ok value
decodeInt _ = Error "Expected an integer"

decodeList :: (Json -> Result Text a) -> Json -> Result Text (List a)
decodeList decodeItem json = case json of
  Json.List items -> Result.collect decodeItem items
  _ -> Error "Expected a list"

list :: Format item -> Format (List item)
list (Format encodeItem decodeItem itemSchema) =
  Format
    { encodeFunction = Json.listOf encodeItem
    , decodeFunction = decodeList decodeItem
    , schema = Json.Schema.array{Json.Schema.items = Just itemSchema}
    }

toNonEmpty :: List item -> Result Text (NonEmpty item)
toNonEmpty (NonEmpty items) = Ok items
toNonEmpty [] = Error "List is empty"

nonEmpty :: Format item -> Format (NonEmpty item)
nonEmpty (Format encodeItem decodeItem itemSchema) =
  Format
    { encodeFunction = Json.listOf encodeItem . NonEmpty.toList
    , decodeFunction = \json -> do
        items <- decodeList decodeItem json
        toNonEmpty items
    , schema = Json.Schema.array{Json.Schema.items = Just itemSchema, Json.Schema.minItems = Just 1}
    }

decodeMap :: (Json -> Result Text a) -> Json -> Result Text (Map Text a)
decodeMap decodeItem json = case json of
  Json.Map fields ->
    Map.toList fields
      & Result.collect (decodeMapField decodeItem)
      & Result.map Map.fromKeyValuePairs
  _ -> Error "Expected a map"

decodeMapField :: (Json -> Result Text a) -> (Text, Json) -> Result Text (Text, a)
decodeMapField decodeItem (name, json) = Result.map (name,) (decodeItem json)

map :: Format item -> Format (Map Text item)
map (Format encodeItem decodeItem itemSchema) =
  Format
    { encodeFunction = Json.map . Map.map encodeItem
    , decodeFunction = decodeMap decodeItem
    , schema = Json.Schema.object{Json.Schema.items = Just itemSchema}
    }

decodeObject :: (Map Text Json -> Result Text a) -> Json -> Result Text a
decodeObject fromFields json = case json of
  Json.Map fields -> fromFields fields
  _ -> Error "Expected an object"

object :: constructor -> Fields constructor parent () -> Format parent
object constructor (Fields decompose compose properties required) =
  Format
    { encodeFunction = Json.map . decompose
    , decodeFunction = decodeObject (\fieldValues -> compose fieldValues constructor)
    , schema = Json.Schema.object{Json.Schema.required, Json.Schema.properties}
    }

singleField :: (child -> parent) -> Field parent child () -> Format parent
singleField constructor field = object constructor (lastField field)

data Field parent child dummy = Field
  { writeField :: parent -> Map Text Json -> Map Text Json
  , readField :: Map Text Json -> Result Text child
  , fieldSchema :: FieldSchema
  }

data Fields constructor parent dummy = Fields
  { decompose :: parent -> Map Text Json
  , compose :: Map Text Json -> constructor -> Result Text parent
  , properties :: Map Text Json.Schema
  , required :: List Text
  }

class FieldComposition a b c | a b -> c where
  (>>) :: a -> b -> c

instance
  parent ~ parent' =>
  FieldComposition
    (Field parent child ())
    (Fields constructor parent' ())
    (Fields (child -> constructor) parent ())
  where
  field >> fields = do
    let (Field writeField readField fieldSchema) = field
    let (Fields decompose compose properties required) = fields
    Fields
      { decompose = \parent -> writeField parent (decompose parent)
      , compose = \fieldValues constructor -> do
          childValue <- readField fieldValues
          compose fieldValues (constructor childValue)
      , properties = Map.set fieldSchema.name fieldSchema.schema properties
      , required = [fieldSchema.name | fieldSchema.required] <> required
      }

instance
  parent ~ parent' =>
  FieldComposition
    (Field parent child1 ())
    (Field parent' child2 ())
    (Fields (child1 -> child2 -> parent) parent ())
  where
  field1 >> field2 = field1 >> lastField field2

lastField :: Field parent child () -> Fields (child -> parent) parent ()
lastField (Field writeField readField fieldSchema) = do
  let decompose parent = writeField parent Map.empty
  let compose fields constructor = Result.map constructor (readField fields)
  let properties = Map.singleton fieldSchema.name fieldSchema.schema
  let required = [fieldSchema.name | fieldSchema.required]
  Fields{decompose, compose, properties, required}

withinField :: Text -> Result Text a -> Result Text a
withinField fieldName decodeResult = case decodeResult of
  Ok value -> Ok value
  Error message -> Error ("In field \"" <> fieldName <> "\":\n" <> Text.indent "  " message)

requiredField :: Text -> (parent -> child) -> Format child -> Field parent child ()
requiredField fieldName getParentField fieldFormat = do
  let Format{encodeFunction = encodeField, decodeFunction = decodeField, schema} = fieldFormat
  let writeField parent fields = Map.set fieldName (encodeField (getParentField parent)) fields
  let readField fields =
        case Map.get fieldName fields of
          Just fieldValue -> withinField fieldName (decodeField fieldValue)
          Nothing -> Error ("Expected a field named \"" <> fieldName <> "\"")
  let fieldSchema = FieldSchema{name = fieldName, required = True, schema}
  Field{writeField, readField, fieldSchema}

optionalField ::
  Text ->
  (parent -> Maybe child) ->
  Format child ->
  Field parent (Maybe child) ()
optionalField fieldName getParentField fieldFormat = do
  let Format{encodeFunction = encodeField, decodeFunction = decodeField, schema} = fieldFormat
  let writeField parent fields =
        case getParentField parent of
          Just fieldValue -> Map.set fieldName (encodeField fieldValue) fields
          Nothing -> fields
  let readField fields =
        case Map.get fieldName fields of
          Just Json.Null -> Ok Nothing
          Just fieldJson -> withinField fieldName (Result.map Just (decodeField fieldJson))
          Nothing -> Ok Nothing
  let fieldSchema = FieldSchema{name = fieldName, required = False, schema}
  Field{writeField, readField, fieldSchema}

angle :: Json.Format Angle
angle =
  title "Angle" $
    description "Units: radians" $
      convert Angle.radians Angle.inRadians number

length :: Json.Format Length
length =
  title "Length" $
    description "Units: meters" $
      convert Length.meters Length.inMeters number

direction2d :: Json.Format (Direction2d space)
direction2d =
  title "Direction2d" $
    description "A direction (unit vector) in 2D space, given by its X and Y components" $
      examples [Direction2d.x] $
        Tolerance.using Quantity.zero $
          lift Vector2d.direction Vector2d.unit $
            object Vector2d OpenSolid.Json.Format.do
              requiredField "x" Vector2d.xComponent number
              requiredField "y" Vector2d.yComponent number

vector2d :: Json.Format (Vector2D space)
vector2d =
  title "Vector2d" $
    description "A displacement vector in 2D space, given by its X and Y components" $
      examples [Vector2D.zero] $
        object Vector2D OpenSolid.Json.Format.do
          requiredField "x" Vector2D.xComponent length
          requiredField "y" Vector2D.yComponent length

point2d :: Json.Format (Point2D space)
point2d =
  title "Point2d" $
    description "A position in 2D space, given by its X and Y coordinates" $
      examples [Point2D.origin] $
        object Point2D OpenSolid.Json.Format.do
          requiredField "x" Point2D.xCoordinate length
          requiredField "y" Point2D.yCoordinate length
