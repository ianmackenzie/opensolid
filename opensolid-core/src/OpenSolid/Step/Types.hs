module OpenSolid.Step.Types
  ( Header (..)
  , Entity (..)
  , SubEntity (..)
  , Attribute (..)
  , TypeName
  , EnumValue
  )
where

import OpenSolid.Binary (ByteString)
import OpenSolid.Prelude
import OpenSolid.Step.EnumValue (EnumValue)
import OpenSolid.Step.TypeName (TypeName)

-- | A header containing metadata about a STEP file.
data Header = Header
  { description :: List Text
  , implementationLevel :: Text
  , fileName :: Text
  , timestamp :: Text
  , author :: List Text
  , organization :: List Text
  , preprocessorVersion :: Text
  , originatingSystem :: Text
  , authorization :: Text
  , schemaIdentifiers :: List Text
  }

{-| A single entity stored in the data section of a STEP file.

An entity may be a point, a curve, a part, an assembly, or even an entire building.
Entities may be 'simple' (having a type and a list of attributes, which can themselves be references to other entities)
or 'complex' (effectively a list of simple entities combined together).
-}
data Entity = SimpleEntity TypeName (List Attribute) | ComplexEntity (List SubEntity)

-- | A sub-entity of a complex entity.
data SubEntity = SubEntity TypeName (List Attribute)

{-| A single attribute of an entity.

This might be  an X coordinate value, a GUID string, or a reference to another entity.
-}
data Attribute
  = DerivedAttribute
  | NullAttribute
  | BoolAttribute Bool
  | IntAttribute Int
  | NumberAttribute Number
  | TextAttribute Text
  | BinaryDataAttribute ByteString
  | EnumAttribute EnumValue
  | ReferenceTo Entity
  | TypedAttribute TypeName Attribute
  | AttributeList (List Attribute)
