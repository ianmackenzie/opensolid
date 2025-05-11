module OpenSolid.Scene3d
  ( Entity
  , Material
  , mesh
  , body
  , group
  , transformBy
  , placeIn
  , relativeTo
  , metal
  , aluminum
  , iron
  , chromium
  , brass
  , copper
  , gold
  , nickel
  , silver
  , titanium
  , nonmetal
  , material
  , toGlb
  , writeGlb
  )
where

import OpenSolid.Array qualified as Array
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Prelude
import OpenSolid.Scene3d.Gltf qualified as Gltf
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units (Meters)
import OpenSolid.Vertex3d qualified as Vertex3d

data Entity space where
  Mesh ::
    Vertex3d.HasNormal vertex (space @ Meters) =>
    Material ->
    Mesh vertex ->
    Entity space
  Group ::
    List (Entity space) ->
    Entity space
  Placed ::
    Frame3d (global @ Meters) (Defines local) ->
    Entity local ->
    Entity global

data Material = Material {baseColor :: Color, roughness :: Float, metallic :: Float}

instance FFI (Entity space) where
  representation = FFI.nestedClassRepresentation "Scene3d" "Entity"

instance FFI Material where
  representation = FFI.nestedClassRepresentation "Scene3d" "Material"

mesh :: Vertex3d.HasNormal vertex (space @ Meters) => Material -> Mesh vertex -> Entity space
mesh = Mesh

{-| Render the given body with the given material.

The body will first be converted to a mesh using the given constraints.
-}
body ::
  Tolerance Meters =>
  NonEmpty (Mesh.Constraint Meters) ->
  Material ->
  Body3d (space @ Meters) ->
  Entity space
body meshConstraints givenMaterial givenBody =
  mesh givenMaterial (Body3d.toMesh meshConstraints givenBody)

{-| Group several entities into a single one.

Useful to allow multiple entities to be transformed as a group.
-}
group :: List (Entity space) -> Entity space
group = Group

transformBy :: Transform3d.Rigid (space @ Meters) -> Entity space -> Entity space
transformBy transform (Placed frame entity) = Placed (Frame3d.transformBy transform frame) entity
transformBy transform entity = Placed (Frame3d.transformBy transform Frame3d.identity) entity

placeIn :: Frame3d (global @ Meters) (Defines local) -> Entity local -> Entity global
placeIn frame (Placed entityFrame entity) = Placed (Frame3d.placeIn frame entityFrame) entity
placeIn frame entity = Placed frame entity

relativeTo :: Frame3d (global @ Meters) (Defines local) -> Entity global -> Entity local
relativeTo frame = placeIn (Frame3d.inverse frame)

-- | Create a metallic material with the given color and roughness.
metal :: Color -> Named "roughness" Float -> Material
metal baseColor (Named roughness) = Material{baseColor, metallic = 1.0, roughness}

-- | Create an aluminum material with the given roughness.
aluminum :: Named "roughness" Float -> Material
aluminum = metal (Color.rgb 0.960 0.961 0.964)

-- | Create a brass material with the given roughness.
brass :: Named "roughness" Float -> Material
brass = metal (Color.rgb 0.949 0.901 0.690)

-- | Create a chromium material with the given roughness.
chromium :: Named "roughness" Float -> Material
chromium = metal (Color.rgb 0.820 0.827 0.834)

-- | Create a copper material with the given roughness.
copper :: Named "roughness" Float -> Material
copper = metal (Color.rgb 0.967 0.866 0.738)

-- | Create a gold material with the given roughness.
gold :: Named "roughness" Float -> Material
gold = metal (Color.rgb 0.975 0.894 0.645)

-- | Create an iron material with the given roughness.
iron :: Named "roughness" Float -> Material
iron = metal (Color.rgb 0.755 0.743 0.733)

-- | Create a nickel material with the given roughness.
nickel :: Named "roughness" Float -> Material
nickel = metal (Color.rgb 0.826 0.804 0.762)

-- | Create a silver material with the given roughness.
silver :: Named "roughness" Float -> Material
silver = metal (Color.rgb 0.983 0.977 0.965)

-- | Create a titanium material with the given roughness.
titanium :: Named "roughness" Float -> Material
titanium = metal (Color.rgb 0.807 0.787 0.764)

-- | Create a non-metallic material with the given color and roughness.
nonmetal :: Color -> Named "roughness" Float -> Material
nonmetal baseColor (Named roughness) = Material{baseColor, roughness, metallic = 0.0}

-- | Create a material with the given base color, metallic factor and roughness.
material :: Color -> Named "metallic" Float -> Named "roughness" Float -> Material
material baseColor (Named metallic) (Named roughness) = Material{baseColor, metallic, roughness}

{-| Convert a scene to binary glTF format.

Same as 'writeGlb' except it just returns the raw binary builder instead of writing to a file.
-}
toGlb :: List (Entity space) -> Builder
toGlb entities = do
  let meshes = gltfMeshes Frame3d.identity (group entities)
  let sceneObject = Json.object [Json.field "nodes" $ Json.listOf Json.int [0 .. List.length meshes - 1]]
  let bufferBuilder = Binary.collect meshBuilder meshes
  let bufferByteLength = Int.sumOf meshByteLength meshes
  let encodedMeshes = encodeMeshes 0 0 meshes
  let bufferObject = Json.object [Json.field "byteLength" $ Json.int bufferByteLength]
  Gltf.toBinary
    [ Json.field "scene" $ Json.int 0
    , Json.field "scenes" $ Json.list [sceneObject]
    , Json.field "nodes" $ Json.listOf meshNode encodedMeshes
    , Json.field "meshes" $ Json.listOf meshObject encodedMeshes
    , Json.field "bufferViews" $ Json.list (List.collect bufferViews encodedMeshes)
    , Json.field "accessors" $ Json.list (List.collect accessors encodedMeshes)
    , Json.field "materials" $ Json.listOf materialObject encodedMeshes
    , Json.field "buffers" $ Json.list [bufferObject]
    ]
    bufferByteLength
    bufferBuilder

{-| Write a scene to a binary glTF file.

The given plane will be used as the ground plane, with:
- the origin of the plane being the global origin,
- the normal direction of the plane being the global up direction (positive Y in glTF), and
- the positive X direction of the plane being the 'forwards' direction (positive Z in glTF).
-}
writeGlb :: Text -> List (Entity space) -> IO ()
writeGlb path givenEntities = IO.writeBinary path (toGlb givenEntities)

data EncodedMesh = EncodedMesh
  { bufferViews :: List Json
  , accessors :: List Json
  , meshObject :: Json
  , meshNode :: Json
  , materialObject :: Json
  }

encodeMeshes :: Int -> Int -> List (GltfMesh space) -> List EncodedMesh
encodeMeshes index offset meshes = case meshes of
  [] -> []
  GltfMesh
    { frame
    , gltfMaterial
    , numFaces
    , indicesByteLength
    , numVertices
    , verticesByteLength
    , minPosition
    , maxPosition
    }
    : rest -> do
      let indicesBufferViewIndex = 2 * index
      let verticesBufferViewIndex = 2 * index + 1
      let indicesAccessorIndex = 3 * index
      let positionAccessorIndex = 3 * index + 1
      let normalAccessorIndex = 3 * index + 2
      let indicesBufferView =
            Json.object
              [ Json.field "buffer" $ Json.int 0
              , Json.field "byteOffset" $ Json.int offset
              , Json.field "byteLength" $ Json.int indicesByteLength
              , Json.field "target" $ Json.int 34963 -- ELEMENT_ARRAY_BUFFER, for vertex indices
              ]
      let verticesBufferView =
            Json.object
              [ Json.field "buffer" $ Json.int 0
              , Json.field "byteOffset" $ Json.int (offset + indicesByteLength)
              , Json.field "byteLength" $ Json.int verticesByteLength
              , Json.field "byteStride" $ Json.int 24
              , Json.field "target" $ Json.int 34962 -- ARRAY_BUFFER, for vertex attributes
              ]
      let indicesAccessor =
            Json.object
              [ Json.field "bufferView" $ Json.int indicesBufferViewIndex
              , Json.field "byteOffset" $ Json.int 0
              , Json.field "type" $ Json.text "SCALAR"
              , Json.field "componentType" $ Json.int 5125 -- UNSIGNED_INT
              , Json.field "count" $ Json.int (3 * numFaces)
              ]
      let positionAccessor =
            Json.object
              [ Json.field "bufferView" $ Json.int verticesBufferViewIndex
              , Json.field "byteOffset" $ Json.int 0
              , Json.field "type" $ Json.text "VEC3"
              , Json.field "componentType" $ Json.int 5126 -- FLOAT
              , Json.field "count" $ Json.int numVertices
              , Json.field "min" $ minPosition
              , Json.field "max" $ maxPosition
              ]
      let normalAccessor =
            Json.object
              [ Json.field "bufferView" $ Json.int verticesBufferViewIndex
              , Json.field "byteOffset" $ Json.int 12
              , Json.field "type" $ Json.text "VEC3"
              , Json.field "componentType" $ Json.int 5126 -- FLOAT
              , Json.field "count" $ Json.int numVertices
              ]
      let attributes =
            Json.object
              [ Json.field "POSITION" $ Json.int positionAccessorIndex
              , Json.field "NORMAL" $ Json.int normalAccessorIndex
              ]
      let meshPrimitive =
            Json.object
              [ Json.field "attributes" $ attributes
              , Json.field "indices" $ Json.int indicesAccessorIndex
              , Json.field "material" $ Json.int index
              ]
      let meshObject = Json.object [Json.field "primitives" $ Json.list [meshPrimitive]]
      let meshNode =
            Json.object
              [ Json.field "mesh" $ Json.int index
              , Gltf.matrixField frame
              ]
      let encodedMesh =
            EncodedMesh
              { bufferViews = [indicesBufferView, verticesBufferView]
              , accessors = [indicesAccessor, positionAccessor, normalAccessor]
              , meshObject
              , meshNode
              , materialObject = gltfMaterial
              }
      encodedMesh : encodeMeshes (index + 1) (offset + indicesByteLength + verticesByteLength) rest

data GltfMesh space where
  GltfMesh ::
    { frame :: Frame3d (space @ Meters) (Defines local)
    , gltfMaterial :: Json
    , numFaces :: Int
    , indices :: Builder
    , indicesByteLength :: Int
    , numVertices :: Int
    , vertices :: Builder
    , verticesByteLength :: Int
    , minPosition :: Json
    , maxPosition :: Json
    } ->
    GltfMesh space

gltfMeshes ::
  Frame3d (global @ Meters) (Defines local) ->
  Entity local ->
  List (GltfMesh global)
gltfMeshes parentFrame entity = case entity of
  Mesh Material{baseColor, metallic, roughness} smoothMesh -> do
    let vertices = Mesh.vertices smoothMesh
    let numVertices = Array.length vertices
    let faceIndices = Mesh.faceIndices smoothMesh
    let numFaces = List.length faceIndices
    let meshBounds = Bounded3d.bounds smoothMesh
    let (xBounds, yBounds, zBounds) = Bounds3d.coordinates Gltf.convention meshBounds
    let Bounds xLow xHigh = xBounds
    let Bounds yLow yHigh = yBounds
    let Bounds zLow zHigh = zBounds
    List.singleton
      GltfMesh
        { frame = parentFrame
        , gltfMaterial = Gltf.pbrMaterial baseColor (#metallic metallic) (#roughness roughness)
        , numFaces
        , indices = Gltf.faceIndices faceIndices
        , indicesByteLength = 3 * 4 * numFaces
        , numVertices
        , vertices = Gltf.pointsAndNormals vertices
        , verticesByteLength = 6 * 4 * numVertices
        , minPosition = Json.listOf (Json.float . Length.inMeters) [xLow, yLow, zLow]
        , maxPosition = Json.listOf (Json.float . Length.inMeters) [xHigh, yHigh, zHigh]
        }
  Group children -> List.collect (gltfMeshes parentFrame) children
  Placed frame child -> gltfMeshes (Frame3d.placeIn parentFrame frame) child

meshBuilder :: GltfMesh space -> Builder
meshBuilder GltfMesh{indices, vertices} = indices <> vertices

meshByteLength :: GltfMesh space -> Int
meshByteLength GltfMesh{indicesByteLength, verticesByteLength} =
  indicesByteLength + verticesByteLength
