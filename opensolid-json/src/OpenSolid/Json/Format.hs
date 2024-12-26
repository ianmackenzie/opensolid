module OpenSolid.Json.Format
  ( Format (..)
  , coerce
  , convert
  , lift
  , float
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

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import Data.Coerce (Coercible)
import Data.Coerce qualified
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Json.FieldSchema (FieldSchema (FieldSchema))
import OpenSolid.Json.FieldSchema qualified as FieldSchema
import OpenSolid.Json.Schema qualified as Json.Schema
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters)

data Format a = Format
  { encode :: a -> Json
  , decode :: Json -> Result Text a
  , schema :: Json.Schema
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
convert up down format = lift (up >> Success) down format

lift :: (a -> Result x b) -> (b -> a) -> Format a -> Format b
lift up down format = do
  let Format{encode, decode, schema} = format
  Format
    { encode = down >> encode
    , decode = decode >> Result.andThen (Result.try . up)
    , schema = removeMetadata schema
    }

bool :: Format Bool
bool =
  Format{encode = Json.bool, decode = decodeBool, schema = Json.Schema.boolean}
    |> title "Bool"
    |> description "A boolean true or false"

decodeBool :: Json -> Result Text Bool
decodeBool (Json.Bool value) = Success value
decodeBool _ = Failure "Expected a boolean"

text :: Format Text
text =
  Format{encode = Json.text, decode = decodeText, schema = Json.Schema.string}
    |> title "Text"
    |> description "Arbitrary text"

decodeText :: Json -> Result Text Text
decodeText (Json.Text value) = Success value
decodeText _ = Failure "Expected text"

float :: Format Float
float =
  Format{encode = Json.float, decode = decodeFloat, schema = Json.Schema.number}
    |> title "Float"
    |> description "A unitless floating-point number"

decodeFloat :: Json -> Result Text Float
decodeFloat (Json.Float value) = Success value
decodeFloat _ = Failure "Expected a number"

int :: Format Int
int =
  Format{encode = Json.int, decode = decodeInt, schema = Json.Schema.integer}
    |> title "Int"
    |> description "An integer"

decodeInt :: Json -> Result Text Int
decodeInt (Json.Int value) = Success value
decodeInt _ = Failure "Expected an integer"

decodeList :: (Json -> Result Text a) -> Json -> Result Text (List a)
decodeList decodeItem json = case json of
  Json.List items -> Result.collect decodeItem items
  _ -> Failure "Expected a list"

list :: Format item -> Format (List item)
list (Format encodeItem decodeItem itemSchema) =
  Format
    { encode = Json.list encodeItem
    , decode = decodeList decodeItem
    , schema = Json.Schema.array{Json.Schema.items = Just itemSchema}
    }

toNonEmpty :: List item -> Result Text (NonEmpty item)
toNonEmpty (NonEmpty items) = Success items
toNonEmpty [] = Failure "List is empty"

nonEmpty :: Format item -> Format (NonEmpty item)
nonEmpty (Format encodeItem decodeItem itemSchema) =
  Format
    { encode = NonEmpty.toList >> Json.list encodeItem
    , decode = decodeList decodeItem >> Result.andThen toNonEmpty
    , schema = Json.Schema.array{Json.Schema.items = Just itemSchema, Json.Schema.minItems = Just 1}
    }

decodeMap :: (Json -> Result Text a) -> Json -> Result Text (Map Text a)
decodeMap decodeItem json = case json of
  Json.Map fields ->
    Map.toList fields
      |> Result.collect (decodeMapField decodeItem)
      |> Result.map Map.fromKeyValuePairs
  _ -> Failure "Expected a map"

decodeMapField :: (Json -> Result Text a) -> (Text, Json) -> Result Text (Text, a)
decodeMapField decodeItem (name, json) = Result.map (name,) (decodeItem json)

map :: Format item -> Format (Map Text item)
map (Format encodeItem decodeItem itemSchema) =
  Format
    { encode = Map.map encodeItem >> Json.map
    , decode = decodeMap decodeItem
    , schema = Json.Schema.object{Json.Schema.items = Just itemSchema}
    }

decodeObject :: (Map Text Json -> Result Text a) -> Json -> Result Text a
decodeObject fromFields json = case json of
  Json.Map fields -> fromFields fields
  _ -> Failure "Expected an object"

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
  , properties :: Map Text Json.Schema
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
withinField fieldName = Result.addContext ("In field \"" + fieldName + "\"")

requiredField :: Text -> (parent -> child) -> Format child -> Field parent child ()
requiredField fieldName getField fieldFormat = do
  let Format{encode = encodeField, decode = decodeField, schema} = fieldFormat
  let write parent fields = Map.set fieldName (encodeField (getField parent)) fields
  let read fields =
        case Map.get fieldName fields of
          Just fieldValue -> withinField fieldName (decodeField fieldValue)
          Nothing -> Failure ("Expected a field named \"" + fieldName + "\"")
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
          Just Json.Null -> Success Nothing
          Just fieldJson -> withinField fieldName (Result.map Just (decodeField fieldJson))
          Nothing -> Success Nothing
  let fieldSchema = FieldSchema{name = fieldName, required = False, schema}
  Field{write, read, fieldSchema}

angle :: Json.Format Angle
angle =
  title "Angle" $
    description "Units: radians" $
      convert Angle.radians Angle.inRadians float

length :: Json.Format Length
length =
  title "Length" $
    description "Units: meters" $
      convert Length.meters Length.inMeters float

direction2d :: Json.Format (Direction2d space)
direction2d =
  title "Direction2d" $
    description "A direction (unit vector) in 2D space, given by its X and Y components" $
      examples [Direction2d.x] $
        Tolerance.exactly $
          lift Vector2d.direction Vector2d.unit $
            object Vector2d.xy do
              requiredField "x" Vector2d.xComponent float
              requiredField "y" Vector2d.yComponent float

vector2d :: Json.Format (Vector2d (space @ Meters))
vector2d =
  title "Vector2d" $
    description "A displacement vector in 2D space, given by its X and Y components" $
      examples [Vector2d.zero] $
        object Vector2d.xy do
          requiredField "x" Vector2d.xComponent length
          requiredField "y" Vector2d.yComponent length

point2d :: Json.Format (Point2d (space @ Meters))
point2d =
  title "Point2d" $
    description "A position in 2D space, given by its X and Y coordinates" $
      examples [Point2d.origin] $
        object Point2d.xy do
          requiredField "x" Point2d.xCoordinate length
          requiredField "y" Point2d.yCoordinate length
