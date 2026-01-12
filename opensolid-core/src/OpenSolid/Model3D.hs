module OpenSolid.Model3D
  ( Model3D (Body, Group)
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

import OpenSolid.Body3D (Body3D)
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.List qualified as List
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Prelude
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.World3D qualified as World3D

{-| A generic hierarchical 3D model for visualization/archival purposes.

A model is composed of bodies (parts) and groups of models (assemblies),
each with optional attributes such as name or material.
-}
data Model3D space where
  BodyNode :: Tolerance Meters => List Attribute -> Body3D space -> Model3D space
  GroupNode :: List Attribute -> List (Model3D space) -> Model3D space

instance FFI (Model3D FFI.Space) where
  representation = FFI.classRepresentation "Model3D"

-- | An attribute that can be applied to a model.
data Attribute
  = Name Text
  | PbrMaterial PbrMaterial
  | Opacity Number

instance FFI Attribute where
  representation = FFI.nestedClassRepresentation "Model3D" "Attribute"

data Context = Context
  { ownName :: Maybe Text
  , parentNames :: List Text
  , ownPbrMaterial :: Maybe PbrMaterial
  , currentPbrMaterial :: PbrMaterial
  , ownOpacity :: Number
  , currentMultipliedOpacity :: Number
  }

type Traversal = ?context :: Context

traversal :: Traversal => Context
traversal = ?context

inspect :: (Traversal => Model3D space -> a) -> Model3D space -> a
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
          , ownOpacity = 1
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
              appliedContext.currentMultipliedOpacity .*. appliedContext.ownOpacity
          }
  let ?context = updatedContext in callback

rootContext :: Context
rootContext =
  Context
    { ownName = Nothing
    , parentNames = []
    , ownPbrMaterial = Nothing
    , currentPbrMaterial = PbrMaterial.aluminum (#roughness 0.2)
    , ownOpacity = 1
    , currentMultipliedOpacity = 1
    }

data BodyWithContext space where
  BodyWithContext ::
    (Traversal, Tolerance Meters) =>
    Body3D space ->
    BodyWithContext space

data GroupWithContext space where
  GroupWithContext :: Traversal => List (Model3D space) -> GroupWithContext space

visitBody :: Traversal => Model3D space -> Maybe (BodyWithContext space)
visitBody GroupNode{} = Nothing
visitBody (BodyNode attrs bod) = inChildContext attrs (Just (BodyWithContext bod))

visitGroup :: Traversal => Model3D space -> Maybe (GroupWithContext space)
visitGroup BodyNode{} = Nothing
visitGroup (GroupNode attrs kids) = inChildContext attrs (Just (GroupWithContext kids))

{-# COMPLETE Body, Group #-}

pattern Body ::
  Traversal =>
  (Traversal, Tolerance Meters) =>
  Body3D space ->
  Model3D space
pattern Body bod <- (visitBody -> Just (BodyWithContext bod))

pattern Group ::
  Traversal =>
  Traversal =>
  List (Model3D space) ->
  Model3D space
pattern Group kids <- (visitGroup -> Just (GroupWithContext kids))

-- | Create a model from a single solid body (a part).
body :: Tolerance Meters => Body3D space -> Model3D space
body = bodyWith []

-- | Create a model from a single solid body (a part), with the given attributes.
bodyWith :: Tolerance Meters => List Attribute -> Body3D space -> Model3D space
bodyWith = BodyNode

-- | Create a model formed from a group of sub-models (an assembly).
group :: List (Model3D space) -> Model3D space
group = groupWith []

-- | Create a model formed from a group of sub-models (an assembly), with the given attributes.
groupWith :: List Attribute -> List (Model3D space) -> Model3D space
groupWith = GroupNode

-- | An empty model.
nothing :: Model3D space
nothing = group []

transformBy :: Transform3D.Rigid space -> Model3D space -> Model3D space
transformBy transform model = placeIn (Frame3D.transformBy transform World3D.frame) model

-- | Convert a model defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Model3D local -> Model3D global
placeIn frame model = case model of
  BodyNode attrs bod -> BodyNode attrs (Body3D.placeIn frame bod)
  GroupNode attrs kids -> GroupNode attrs (List.map (placeIn frame) kids)

-- | Convert a model defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Model3D global -> Model3D local
relativeTo frame = placeIn (Frame3D.inverse frame)

-- | Add the given attributes to the given model.
withAttributes :: List Attribute -> Model3D space -> Model3D space
withAttributes givenAttributes model = case model of
  BodyNode existingAttributes bod -> BodyNode (existingAttributes <> givenAttributes) bod
  GroupNode existingAttributes kids -> GroupNode (existingAttributes <> givenAttributes) kids

with :: Attribute -> Model3D space -> Model3D space
with attribute = withAttributes [attribute]

-- | Create a name attribute.
name :: Text -> Attribute
name = Name

-- | Set the name of a model.
withName :: Text -> Model3D space -> Model3D space
withName value = with (name value)

-- | Create a PBR material attribute for rendering.
pbrMaterial :: PbrMaterial -> Attribute
pbrMaterial = PbrMaterial

-- | Set the PBR material used by a model.
withPbrMaterial :: PbrMaterial -> Model3D space -> Model3D space
withPbrMaterial value = with (pbrMaterial value)

-- | Create an opacity attribute, where 0 is fully transparent and 1 is fully opaque.
opacity :: Number -> Attribute
opacity = Opacity

-- | Set the opacity of a model, where 0 is fully transparent and 1 is fully opaque.
withOpacity :: Number -> Model3D space -> Model3D space
withOpacity value = with (opacity value)
