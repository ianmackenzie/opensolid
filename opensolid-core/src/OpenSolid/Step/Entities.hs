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

data EntityMap = EntityMap
  { nextId :: Int
  , idMap :: Map ByteString Int
  }

emptyMap :: EntityMap
emptyMap = EntityMap{nextId = 1, idMap = Map.empty}

buildMap :: List Entity -> EntityMap
buildMap entities = emptyMap & forEach entities \entity -> Pair.second . addEntity entity

encodeEntityRecord :: (TypeName, List Builder) -> Builder
encodeEntityRecord (typeName, encodedAttributes) =
  Step.Encode.typeName typeName <> Step.Encode.list encodedAttributes

addEntity :: Entity -> EntityMap -> (Int, EntityMap)
addEntity entity entityMap =
  case entity of
    SimpleEntity typeName attributes -> do
      let (attributeValues, mapWithAttributes) = addAttributes attributes entityMap
      let encodedEntity = encodeEntityRecord (typeName, attributeValues)
      update encodedEntity mapWithAttributes
    ComplexEntity subEntities -> do
      let (simpleEntityValues, mapWithSimpleEntities) = addEntityRecords subEntities entityMap []
      let encodedSimpleEntities = List.map encodeEntityRecord simpleEntityValues
      let openingParenthesis = Data.ByteString.Builder.charUtf8 '('
      let closingParenthesis = Data.ByteString.Builder.charUtf8 ')'
      let encodedEntity =
            openingParenthesis <> Binary.concat encodedSimpleEntities <> closingParenthesis
      update encodedEntity mapWithSimpleEntities

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
addAttributes attributes entityMap = do
  let (finalAttributeValues, finalMap) =
        ([], entityMap) & do
          forEach attributes \attribute (currentAttributeValues, currentMap) -> do
            let (attributeValue, updatedMap) = addAttribute attribute currentMap
            let updatedAttributeValues = attributeValue : currentAttributeValues
            (updatedAttributeValues, updatedMap)
  (List.reverse finalAttributeValues, finalMap)

addAttribute :: Attribute -> EntityMap -> (Builder, EntityMap)
addAttribute attribute entityMap =
  case attribute of
    DerivedAttribute -> (Step.Encode.derived, entityMap)
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

update :: Builder -> EntityMap -> (Int, EntityMap)
update encodedEntity entityMap = do
  let entityBytes = Binary.bytes encodedEntity
  case Map.get entityBytes entityMap.idMap of
    Just entityId -> (entityId, entityMap)
    Nothing -> do
      let entityId = entityMap.nextId
      let updatedMap =
            EntityMap
              { nextId = entityId + 1
              , idMap = entityMap.idMap & Map.set entityBytes entityId
              }
      (entityId, updatedMap)

compile :: List Entity -> List (Int, ByteString)
compile entities = do
  let entityMap = buildMap entities
  entityMap.idMap & Map.toList & List.map Pair.flip & List.sortBy Pair.first
