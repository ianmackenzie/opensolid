module OpenSolid.Step.Entities (compile) where

import Data.ByteString.Builder qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Binary qualified as Binary
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Step.Encode qualified as Step.Encode
import OpenSolid.Step.TypeName (TypeName)
import OpenSolid.Step.Types (Attribute (..), Entity (..), SubEntity (SubEntity))

data EntityMap = EntityMap Int (Map ByteString (Int, Entity))

buildMap :: List Entity -> EntityMap
buildMap entities =
  List.foldl
    (\accumulatedMap entity -> Pair.second (addEntity entity accumulatedMap))
    (EntityMap 1 Map.empty)
    entities

encodeEntityRecord :: (TypeName, List Builder) -> Builder
encodeEntityRecord (typeName, encodedAttributes) =
  Step.Encode.typeName typeName <> Step.Encode.list encodedAttributes

addEntity :: Entity -> EntityMap -> (Int, EntityMap)
addEntity entity entityMap =
  case entity of
    SimpleEntity typeName attributes -> do
      let (attributeValues, mapWithAttributes) = addAttributes attributes entityMap
      let encodedEntity = encodeEntityRecord (typeName, attributeValues)
      update entity encodedEntity mapWithAttributes
    ComplexEntity subEntities -> do
      let (simpleEntityValues, mapWithSimpleEntities) = addEntityRecords subEntities entityMap []
      let encodedSimpleEntities = List.map encodeEntityRecord simpleEntityValues
      let openingParenthesis = Data.ByteString.Builder.charUtf8 '('
      let closingParenthesis = Data.ByteString.Builder.charUtf8 ')'
      let encodedEntity =
            openingParenthesis <> Binary.concat encodedSimpleEntities <> closingParenthesis
      update entity encodedEntity mapWithSimpleEntities

addEntityRecords ::
  List SubEntity ->
  EntityMap ->
  List (TypeName, List Builder) ->
  (List (TypeName, List Builder), EntityMap)
addEntityRecords entityRecords entityMap accumulated =
  case entityRecords of
    (SubEntity typeName attributes) : rest -> do
      let (attributeValues, mapWithAttributes) = addAttributes attributes entityMap
      addEntityRecords rest mapWithAttributes ((typeName, attributeValues) : accumulated)
    [] -> (List.reverse accumulated, entityMap)

addAttributes :: List Attribute -> EntityMap -> (List Builder, EntityMap)
addAttributes attributes entityMap =
  List.foldl
    ( \(accumulatedAttributeValues, accumulatedMap) attribute -> do
        let (attributeValue, mapWithAttribute) = addAttribute attribute accumulatedMap
        (attributeValue : accumulatedAttributeValues, mapWithAttribute)
    )
    ([], entityMap)
    attributes
    & Pair.mapFirst List.reverse

addAttribute :: Attribute -> EntityMap -> (Builder, EntityMap)
addAttribute attribute entityMap =
  case attribute of
    DerivedValue -> (Step.Encode.derivedValue, entityMap)
    NullAttribute -> (Step.Encode.null, entityMap)
    BoolAttribute bool -> (Step.Encode.bool bool, entityMap)
    IntAttribute int -> (Step.Encode.int int, entityMap)
    NumberAttribute number -> (Step.Encode.number number, entityMap)
    TextAttribute text -> (Step.Encode.text text, entityMap)
    BinaryDataAttribute bytes -> do
      let builder = Data.ByteString.Builder.byteString bytes
      (Step.Encode.binaryData builder, entityMap)
    EnumAttribute enumValue -> (Step.Encode.enum enumValue, entityMap)
    ReferenceTo entity -> do
      let (entityId, updatedMap) = addEntity entity entityMap
      (Step.Encode.id entityId, updatedMap)
    TypedAttribute typeName typedAttribute -> do
      let (attributeValue, updatedMap) = addAttribute typedAttribute entityMap
      (Step.Encode.typedAttribute typeName attributeValue, updatedMap)
    AttributeList attributes -> do
      let (attributeValues, mapWithAttributes) = addAttributes attributes entityMap
      (Step.Encode.list attributeValues, mapWithAttributes)

update :: Entity -> Builder -> EntityMap -> (Int, EntityMap)
update entity encodedEntity entityMap = do
  let EntityMap nextId idMap = entityMap
  let entityBytes = Binary.bytes encodedEntity
  case Map.get entityBytes idMap of
    Just (entityId, _) -> (entityId, entityMap)
    Nothing -> (nextId, EntityMap (nextId + 1) (Map.set entityBytes (nextId, entity) idMap))

compile :: List Entity -> List (Int, Entity, ByteString)
compile entities = do
  let (EntityMap _ idMap) = buildMap entities
  Map.toList idMap
    & List.map (\(entityBytes, (entityId, entity)) -> (entityId, entity, entityBytes))
    & List.sortBy (\(entityId, _, _) -> entityId)
