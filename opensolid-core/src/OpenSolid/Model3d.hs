module OpenSolid.Model3d
  ( Model3d (Body, Group)
  , Attribute
  , Context (..)
  , traversal
  , inspect
  , Traversal
  , body
  , bodyWith
  , group
  , groupWith
  , nothing
  , transformBy
  , placeIn
  , relativeTo
  , withAttributes
  , name
  , withName
  , pbrMaterial
  , withPbrMaterial
  , opacity
  , withOpacity
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
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Prelude
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.World3d qualified as World3d

{-| A generic hierarchical 3D model for visualization/archival purposes.

A model is composed of bodies (parts) and groups of models (assemblies),
each with optional attributes such as name or material.
-}
data Model3d space where
  BodyNode :: Tolerance Meters => List Attribute -> Body3d (space @ Meters) -> Model3d space
  GroupNode :: List Attribute -> List (Model3d space) -> Model3d space

instance FFI (Model3d FFI.Space) where
  representation = FFI.classRepresentation "Model3d"

-- | An attribute that can be applied to a model.
data Attribute
  = Name Text
  | PbrMaterial PbrMaterial
  | Opacity Float

instance FFI Attribute where
  representation = FFI.nestedClassRepresentation "Model3d" "Attribute"

data Context = Context
  { ownName :: Maybe Text
  , parentNames :: List Text
  , ownPbrMaterial :: Maybe PbrMaterial
  , currentPbrMaterial :: PbrMaterial
  , ownOpacity :: Float
  , currentMultipliedOpacity :: Float
  }

type Traversal = ?context :: Context

traversal :: Traversal => Context
traversal = ?context

inspect :: (Traversal => Model3d space -> a) -> Model3d space -> a
inspect function model = let ?context = rootContext in function model

applyAttribute :: Context -> Attribute -> Context
applyAttribute ctx attr = case attr of
  Name value -> ctx{ownName = Just value}
  PbrMaterial value -> ctx{ownPbrMaterial = Just value, currentPbrMaterial = value}
  -- Note that here we *don't* update currentMultipliedOpacity here,
  -- to avoid multiplying it multiple times just in case there are multiple opacity attributes
  -- (it will be updated below, at the end of inChildContext)
  Opacity value -> ctx{ownOpacity = value}

inChildContext :: Traversal => List Attribute -> (Traversal => a) -> a
inChildContext childAttributes callback = do
  let parentContext = ?context
  let initialContext =
        Context
          { ownName = Nothing
          , parentNames = parentContext.parentNames <> List.maybe parentContext.ownName
          , ownPbrMaterial = Nothing
          , currentPbrMaterial = parentContext.currentPbrMaterial
          , ownOpacity = 1.0
          , currentMultipliedOpacity = parentContext.currentMultipliedOpacity
          }
  let appliedContext = List.foldl applyAttribute initialContext childAttributes
  -- After applying attribute values, update the current multiplied opacity
  -- (just in case the current node has multiple opacity attribute values,
  -- we should only multiply the current opacity by the *last*/active one;
  -- otherwise we could update currentMultipliedOpacity directly within applyAttribute)
  let updatedContext =
        appliedContext
          { currentMultipliedOpacity =
              appliedContext.currentMultipliedOpacity * appliedContext.ownOpacity
          }
  let ?context = updatedContext in callback

rootContext :: Context
rootContext =
  Context
    { ownName = Nothing
    , parentNames = []
    , ownPbrMaterial = Nothing
    , currentPbrMaterial = PbrMaterial.aluminum (#roughness 0.2)
    , ownOpacity = 1.0
    , currentMultipliedOpacity = 1.0
    }

data BodyWithContext space where
  BodyWithContext ::
    (Traversal, Tolerance Meters) =>
    Body3d (space @ Meters) ->
    BodyWithContext space

data GroupWithContext space where
  GroupWithContext :: Traversal => List (Model3d space) -> GroupWithContext space

visitBody :: Traversal => Model3d space -> Maybe (BodyWithContext space)
visitBody GroupNode{} = Nothing
visitBody (BodyNode attrs bod) = inChildContext attrs (Just (BodyWithContext bod))

visitGroup :: Traversal => Model3d space -> Maybe (GroupWithContext space)
visitGroup BodyNode{} = Nothing
visitGroup (GroupNode attrs kids) = inChildContext attrs (Just (GroupWithContext kids))

{-# COMPLETE Body, Group #-}

pattern Body :: Traversal => (Traversal, Tolerance Meters) => Body3d (space @ Meters) -> Model3d space
pattern Body bod <- (visitBody -> Just (BodyWithContext bod))

pattern Group :: Traversal => Traversal => List (Model3d space) -> Model3d space
pattern Group kids <- (visitGroup -> Just (GroupWithContext kids))

-- | Create a model from a single solid body (a part).
body :: Tolerance Meters => Body3d (space @ Meters) -> Model3d space
body = bodyWith []

-- | Create a model from a single solid body (a part), with the given attributes.
bodyWith :: Tolerance Meters => List Attribute -> Body3d (space @ Meters) -> Model3d space
bodyWith = BodyNode

-- | Create a model formed from a group of sub-models (an assembly).
group :: List (Model3d space) -> Model3d space
group = groupWith []

-- | Create a model formed from a group of sub-models (an assembly), with the given attributes.
groupWith :: List Attribute -> List (Model3d space) -> Model3d space
groupWith = GroupNode

-- | An empty model.
nothing :: Model3d space
nothing = group []

transformBy :: Transform3d.Rigid (space @ Meters) -> Model3d space -> Model3d space
transformBy transform model = placeIn (Frame3d.transformBy transform World3d.frame) model

-- | Convert a model defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3d (global @ Meters) (Defines local) -> Model3d local -> Model3d global
placeIn frame model = case model of
  BodyNode attrs bod -> BodyNode attrs (Body3d.placeIn frame bod)
  GroupNode attrs kids -> GroupNode attrs (List.map (placeIn frame) kids)

-- | Convert a model defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3d (global @ Meters) (Defines local) -> Model3d global -> Model3d local
relativeTo frame = placeIn (Frame3d.inverse frame)

-- | Add the given attributes to the given model.
withAttributes :: List Attribute -> Model3d space -> Model3d space
withAttributes givenAttributes model = case model of
  BodyNode existingAttributes bod -> BodyNode (existingAttributes <> givenAttributes) bod
  GroupNode existingAttributes kids -> GroupNode (existingAttributes <> givenAttributes) kids

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

-- | Create an opacity attribute, where 0 is fully transparent and 1 is fully opaque.
opacity :: Float -> Attribute
opacity = Opacity

-- | Set the opacity of a model, where 0 is fully transparent and 1 is fully opaque.
withOpacity :: Float -> Model3d space -> Model3d space
withOpacity value = with (opacity value)
