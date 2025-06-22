module OpenSolid.Model3d
  ( Model3d (..)
  , Attribute (..)
  , body
  , bodyWith
  , group
  , groupWith
  , nothing
  , transformBy
  , placeIn
  , relativeTo
  , attributes
  , withAttributes
  , name
  , withName
  , pbrMaterial
  , withPbrMaterial
  )
where

import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.List qualified as List
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.Prelude
import OpenSolid.Transform3d qualified as Transform3d

{-| A generic hierarchical 3D model for visualization/archival purposes.

A model is composed of bodies (parts) and groups of models (assemblies),
each with optional attributes such as name or material.
-}
data Model3d space where
  Body :: Tolerance Meters => List Attribute -> Body3d (space @ Meters) -> Model3d space
  Group :: List Attribute -> List (Model3d space) -> Model3d space

-- | An attribute that can be applied to a model.
data Attribute
  = Name Text
  | PbrMaterial PbrMaterial

instance HasField "name" (Model3d space) (Maybe Text) where
  getField = getAttribute (\case Name value -> Just value; _ -> Nothing)

instance HasField "pbrMaterial" (Model3d space) (Maybe PbrMaterial) where
  getField = getAttribute (\case PbrMaterial value -> Just value; _ -> Nothing)

instance FFI (Model3d space) where
  representation = FFI.classRepresentation "Model3d"

instance FFI Attribute where
  representation = FFI.nestedClassRepresentation "Model3d" "Attribute"

getAttribute :: (Attribute -> Maybe a) -> Model3d space -> Maybe a
getAttribute extract model = findAttribute extract (attributes model)

findAttribute :: (Attribute -> Maybe a) -> List Attribute -> Maybe a
findAttribute extract attrs = case attrs of
  [] -> Nothing
  first : rest -> case extract first of
    Just value -> Just value
    Nothing -> findAttribute extract rest

-- | Create a model from a single solid body (a part).
body :: Tolerance Meters => Body3d (space @ Meters) -> Model3d space
body = bodyWith []

-- | Create a model from a single solid body (a part), with the given attributes.
bodyWith :: Tolerance Meters => List Attribute -> Body3d (space @ Meters) -> Model3d space
bodyWith = Body

-- | Create a model formed from a group of sub-models (an assembly).
group :: List (Model3d space) -> Model3d space
group = groupWith []

-- | Create a model formed from a group of sub-models (an assembly), with the given attributes.
groupWith :: List Attribute -> List (Model3d space) -> Model3d space
groupWith = Group

-- | An empty model.
nothing :: Model3d space
nothing = group []

transformBy :: Transform3d.Rigid (space @ Meters) -> Model3d space -> Model3d space
transformBy transform model = placeIn (Frame3d.transformBy transform Frame3d.world) model

placeIn :: Frame3d (global @ Meters) (Defines local) -> Model3d local -> Model3d global
placeIn frame model = case model of
  Body attrs bod -> Body attrs (Body3d.placeIn frame bod)
  Group attrs children -> Group attrs (List.map (placeIn frame) children)

relativeTo :: Frame3d (global @ Meters) (Defines local) -> Model3d global -> Model3d local
relativeTo frame = placeIn (Frame3d.inverse frame)

-- | Get all attributes of a given model.
attributes :: Model3d space -> List Attribute
attributes model = case model of
  Body attrs _ -> attrs
  Group attrs _ -> attrs

-- | Add the given attributes to the given model.
withAttributes :: List Attribute -> Model3d space -> Model3d space
withAttributes givenAttributes model = case model of
  Body existingAttributes bod -> Body (givenAttributes <> existingAttributes) bod
  Group existingAttributes children -> Group (givenAttributes <> existingAttributes) children

with :: Attribute -> Model3d space -> Model3d space
with attribute = withAttributes [attribute]

-- | Create a name attribute.
name :: Text -> Attribute
name = Name

-- | Set the name of a model.
withName :: Text -> Model3d space -> Model3d space
withName value = with (name value)

-- | Create a PBR material attribute for rendering.
pbrMaterial :: PbrMaterial -> Attribute
pbrMaterial = PbrMaterial

-- | Set the PBR material used by a model.
withPbrMaterial :: PbrMaterial -> Model3d space -> Model3d space
withPbrMaterial value = with (pbrMaterial value)
