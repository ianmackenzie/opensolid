module OpenSolid.Step
  ( Header
  , Entity
  , SubEntity
  , Attribute
  , builder
  , write
  , header
  , entity
  , complexEntity
  , subEntity
  , derived
  , null
  , optional
  , bool
  , int
  , number
  , text
  , referenceTo
  , enum
  , binaryData
  , list
  , tuple2
  , tuple3
  , boolAs
  , intAs
  , numberAs
  , textAs
  , enumAs
  , binaryDataAs
  , listAs
  , typedAttribute
  )
where

import Data.ByteString.Builder qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Step.Encode qualified as Step.Encode
import OpenSolid.Step.Entities qualified as Step.Entities
import OpenSolid.Step.EnumValue qualified as Step.EnumValue
import OpenSolid.Step.TypeName qualified as Step.TypeName
import OpenSolid.Step.Types
  ( Attribute (..)
  , Entity (ComplexEntity, SimpleEntity)
  , Header (..)
  , SubEntity (SubEntity)
  )
import OpenSolid.Text qualified as Text

-- | Write a STEP file with the given header and entities to the given path.
write :: Text -> Header -> List Entity -> IO ()
write path givenHeader entities = IO.writeBinary path (builder givenHeader entities)

char :: Char -> Builder
char = Data.ByteString.Builder.charUtf8

encodeLines :: List Builder -> Builder
encodeLines = Binary.join (char '\n')

encodeHeader :: Header -> Builder
encodeHeader givenHeader = do
  let fileDescriptionEntity =
        entity "FILE_DESCRIPTION" $
          [ list text givenHeader.description
          , text givenHeader.implementationLevel
          ]
  let fileNameEntity =
        entity "FILE_NAME" $
          [ text givenHeader.fileName
          , text givenHeader.timestamp
          , list text givenHeader.author
          , list text givenHeader.organization
          , text givenHeader.preprocessorVersion
          , text givenHeader.originatingSystem
          , text givenHeader.authorization
          ]
  let fileSchemaEntity = entity "FILE_SCHEMA" [list text givenHeader.schemaIdentifiers]
  let headerEntities = [fileDescriptionEntity, fileNameEntity, fileSchemaEntity]
  let encodeEntityLine (_, _, encodedEntity) =
        Data.ByteString.Builder.byteString encodedEntity <> char ';'
  let compiledEntities = Step.Entities.compile headerEntities
  encodeLines (List.map encodeEntityLine compiledEntities)

{-| Encode a complete STEP file from a header and a list of entities.

Entities will be assigned integer IDs automatically,
and nested entities (entities that reference other entities)
will be 'flattened' into separate entities referring to each other by their IDs.

Note that it is not actually necessary to list all entities explicitly, only
top-level ones; any entities that are referenced by entities in the given list
will also get included in the output.
-}
builder :: Header -> List Entity -> Builder
builder givenHeader entities = do
  let compiledEntities = Step.Entities.compile entities
  let encodeEntityLine (entityId, _, encodedEntity) =
        Binary.concat
          [ Step.Encode.id entityId
          , char '='
          , Data.ByteString.Builder.byteString encodedEntity
          , char ';'
          ]
  encodeLines
    [ Text.toUtf8 "ISO-10303-21;"
    , Text.toUtf8 "HEADER;"
    , encodeHeader givenHeader
    , Text.toUtf8 "ENDSEC;"
    , Text.toUtf8 "DATA;"
    , encodeLines (List.map encodeEntityLine compiledEntities)
    , Text.toUtf8 "ENDSEC;"
    , Text.toUtf8 "END-ISO-10303-21;\n"
    ]

{-| Create a STEP file header.

  - 'description' should be an informal description of the contents of the file.
  - 'implementationLevel' will typically be "2;1" indicating version 2 of
    ISO 10303, conformance class 1 (which in turn means that the file has a
    single data section and no anchor or reference sections, along with several
    other restrictions). For other possible values, see section 8.2.2 of ISO
    10303-21.
  - 'fileName' may be the file name of the actual file, or it may be an abstract
    name for the contents of the file used when cross-referencing between files.
  - 'timeStamp' should be an
    [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)-formatted date and time.
  - 'author' should include the name and address of the person who created the file.
  - 'organization' should be the organization that the author is associated with.
  - One of 'preprocessorVersion' or 'originatingSystem' should identify what CAD
    program was used to generate the file. This does not seem to be used
    terribly consistently!
  - 'authorization' should include the name and address of whoever authorized
    sending the file.
  - 'schemaIdentifiers' identifies the EXPRESS schema used by entities in the
    file. This will usually be a list containing a single string, which may be
    either a simple string like "IFC2X3" or an identifier such as
    "AUTOMOTIVE_DESIGN { 1 0 10303 214 1 1 1 1 }" (more commonly known as AP214).
-}
header ::
  ("description" ::: List Text) ->
  ("implementationLevel" ::: Text) ->
  ("fileName" ::: Text) ->
  ("timestamp" ::: Text) ->
  ("author" ::: List Text) ->
  ("organization" ::: List Text) ->
  ("preprocessorVersion" ::: Text) ->
  ("originatingSystem" ::: Text) ->
  ("authorization" ::: Text) ->
  ("schemaIdentifiers" ::: List Text) ->
  Header
header
  ("description" ::: givenDescription)
  ("implementationLevel" ::: givenImplementationLevel)
  ("fileName" ::: givenFileName)
  ("timestamp" ::: givenTimestamp)
  ("author" ::: givenAuthor)
  ("organization" ::: givenOrganization)
  ("preprocessorVersion" ::: givenPreprocessorVersion)
  ("originatingSystem" ::: givenOriginatingSystem)
  ("authorization" ::: givenAuthorization)
  ("schemaIdentifiers" ::: givenSchemaIdentifiers) =
    Header
      { description = givenDescription
      , implementationLevel = givenImplementationLevel
      , fileName = givenFileName
      , timestamp = givenTimestamp
      , author = givenAuthor
      , organization = givenOrganization
      , preprocessorVersion = givenPreprocessorVersion
      , originatingSystem = givenOriginatingSystem
      , authorization = givenAuthorization
      , schemaIdentifiers = givenSchemaIdentifiers
      }

{-| Construct a single simple entity with the given type and attributes.

The type name will be capitalized if necessary.
If a given entity is _only_ referred to by a single other entity, you can create
it directly inside the definition of the parent entity.
-}
entity :: Text -> List Attribute -> Entity
entity givenTypeName givenAttributes =
  SimpleEntity (Step.TypeName.fromText givenTypeName) givenAttributes

-- | Construct a single complex entity.
complexEntity :: List SubEntity -> Entity
complexEntity = ComplexEntity

-- | Construct a sub-entity that will become part of a complex entity.
subEntity :: Text -> List Attribute -> SubEntity
subEntity givenTypeName givenAttributes =
  SubEntity (Step.TypeName.fromText givenTypeName) givenAttributes

{-| Construct a reference to another STEP entity.

This will end up being encoded using an integer ID in the resulting STEP file.
-}
referenceTo :: Entity -> Attribute
referenceTo = ReferenceTo

-- | The special 'derived value' attribute.
derived :: Attribute
derived = DerivedAttribute

-- | The special 'null value' attribute.
null :: Attribute
null = NullAttribute

{-| Construct an optional value.

This will use the given attribute constructor if the given value is a 'Just value',
or set the attribute to null if it is 'Nothing'.
-}
optional :: (a -> Attribute) -> Maybe a -> Attribute
optional encoder (Just value) = encoder value
optional _ Nothing = null

-- | Construct a boolean-valued attribute.
bool :: Bool -> Attribute
bool = BoolAttribute

-- | Construct an integer-valued attribute.
int :: Int -> Attribute
int = IntAttribute

-- | Construct a number-valued attribute.
number :: Number -> Attribute
number = NumberAttribute

-- | Construct a text-valued attribute.
text :: Text -> Attribute
text = TextAttribute

{-| Construct an attribute that refers to an enumeration value defined in an EXPRESS schema.

Enumeration values are always encoded as all-caps with leading and trailing periods, like '.STEEL.'.

This function will capitalize and add periods if necessary.
-}
enum :: Text -> Attribute
enum = EnumAttribute . Step.EnumValue.fromText

-- | Construct an attribute from some binary data.
binaryData :: Builder -> Attribute
binaryData bytesBuilder = BinaryDataAttribute (Binary.bytes bytesBuilder)

-- | Construct an attribute which is itself a list of other attributes.
list :: (a -> Attribute) -> List a -> Attribute
list toAttribute values = AttributeList (List.map toAttribute values)

-- | Encode a tuple of two values as a list using the given encoding function.
tuple2 :: (a -> Attribute) -> (a, a) -> Attribute
tuple2 toAttribute (first, second) = list toAttribute [first, second]

-- | Encode a tuple of three values as a list using the given encoding function.
tuple3 :: (a -> Attribute) -> (a, a, a) -> Attribute
tuple3 toAttribute (first, second, third) = list toAttribute [first, second, third]

-- | Construct a type-tagged boolean-valued attribute.
boolAs :: Text -> Bool -> Attribute
boolAs givenTypeName value = typedAttribute givenTypeName (bool value)

-- | Construct a type-tagged integer-valued attribute.
intAs :: Text -> Int -> Attribute
intAs givenTypeName value = typedAttribute givenTypeName (int value)

-- | Construct a type-tagged number-valued attribute.
numberAs :: Text -> Number -> Attribute
numberAs givenTypeName value = typedAttribute givenTypeName (number value)

-- | Construct a type-tagged text-valued attribute.
textAs :: Text -> Text -> Attribute
textAs givenTypeName value = typedAttribute givenTypeName (text value)

-- | Construct a type-tagged enumeration attribute.
enumAs :: Text -> Text -> Attribute
enumAs givenTypeName value = typedAttribute givenTypeName (enum value)

-- | Construct a type-tagged binary-valued attribute.
binaryDataAs :: Text -> Builder -> Attribute
binaryDataAs givenTypeName bytesBuilder = typedAttribute givenTypeName (binaryData bytesBuilder)

-- | Construct a type-tagged list attribute.
listAs :: Text -> (a -> Attribute) -> List a -> Attribute
listAs givenTypeName toAttribute values = typedAttribute givenTypeName (list toAttribute values)

{-| Construct a generic type-tagged attribute.

In most cases it will be simpler to use one of the specific functions
for creating type-tagged attributes.
-}
typedAttribute :: Text -> Attribute -> Attribute
typedAttribute givenTypeName attribute =
  TypedAttribute (Step.TypeName.fromText givenTypeName) attribute
