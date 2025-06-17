module OpenSolid.Scene3d
  ( Scene3d
  , nothing
  , mesh
  , body
  , group
  , transformBy
  , placeIn
  , relativeTo
  , toGlb
  , writeGlb
  )
where

import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Body3d (Body3d)
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds3d qualified as Bounds3d
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
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.Prelude
import OpenSolid.Resolution (Resolution)
import OpenSolid.Scene3d.Gltf qualified as Gltf
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vertex3d qualified as Vertex3d

-- | A collection of graphical objects in 3D space.
data Scene3d space where
  Mesh ::
    Vertex3d.HasNormal vertex (space @ Meters) =>
    PbrMaterial ->
    Mesh vertex ->
    Scene3d space
  Group ::
    List (Scene3d space) ->
    Scene3d space
  Placed ::
    Frame3d (global @ Meters) (Defines local) ->
    Scene3d local ->
    Scene3d global

instance FFI (Scene3d space) where
  representation = FFI.classRepresentation "Scene3d"

-- | An empty scene.
nothing :: Scene3d space
nothing = group []

mesh :: Vertex3d.HasNormal vertex (space @ Meters) => PbrMaterial -> Mesh vertex -> Scene3d space
mesh = Mesh

{-| Render the given body with the given material.

The body will first be converted to a mesh using the given constraints.
-}
body ::
  Tolerance Meters =>
  Resolution Meters ->
  PbrMaterial ->
  Body3d (space @ Meters) ->
  Scene3d space
body resolution givenMaterial givenBody =
  mesh givenMaterial (Body3d.toMesh resolution givenBody)

{-| Create a composite scene out of several sub-scenes.

Sometimes useful to allow multiple objects to be transformed as one.
-}
group :: List (Scene3d space) -> Scene3d space
group = Group

transformBy :: Transform3d.Rigid (space @ Meters) -> Scene3d space -> Scene3d space
transformBy transform (Placed frame scene) = Placed (Frame3d.transformBy transform frame) scene
transformBy transform scene = Placed (Frame3d.transformBy transform Frame3d.world) scene

placeIn :: Frame3d (global @ Meters) (Defines local) -> Scene3d local -> Scene3d global
placeIn frame (Placed sceneFrame scene) = Placed (Frame3d.placeIn frame sceneFrame) scene
placeIn frame scene = Placed frame scene

relativeTo :: Frame3d (global @ Meters) (Defines local) -> Scene3d global -> Scene3d local
relativeTo frame = placeIn (Frame3d.inverse frame)

{-| Convert a scene to binary glTF format.

Same as 'writeGlb' except it just returns the raw binary builder instead of writing to a file.
-}
toGlb :: Scene3d space -> Builder
toGlb scene = do
  let meshes = gltfMeshes Frame3d.world scene
  let sceneObject = Json.object [Json.field "nodes" $ Json.listOf Json.int [0 .. meshes.length - 1]]
  let bufferBuilder = Binary.collect meshBuilder meshes
  let bufferByteLength = Int.sumOf meshByteLength meshes
  let encodedMeshes = encodeMeshes 0 0 meshes
  let bufferObject = Json.object [Json.field "byteLength" $ Json.int bufferByteLength]
  Gltf.toBinary
    [ Json.field "scene" $ Json.int 0
    , Json.field "scenes" $ Json.list [sceneObject]
    , Json.field "nodes" $ Json.listOf (.meshNode) encodedMeshes
    , Json.field "meshes" $ Json.listOf (.meshObject) encodedMeshes
    , Json.field "bufferViews" $ Json.list (List.collect (.bufferViews) encodedMeshes)
    , Json.field "accessors" $ Json.list (List.collect (.accessors) encodedMeshes)
    , Json.field "materials" $ Json.listOf (.materialObject) encodedMeshes
    , Json.field "buffers" $ Json.list [bufferObject]
    ]
    bufferByteLength
    bufferBuilder

-- | Write a scene to a binary glTF file.
writeGlb :: Text -> Scene3d space -> IO ()
writeGlb path scene = IO.writeBinary path (toGlb scene)

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
  Scene3d local ->
  List (GltfMesh global)
gltfMeshes parentFrame scene = case scene of
  Mesh pbrMaterial smoothMesh -> do
    let vertices = smoothMesh.vertices
    let numVertices = vertices.length
    let faceIndices = smoothMesh.faceIndices
    let numFaces = faceIndices.length
    let meshBounds = Bounded3d.bounds smoothMesh
    let (xBounds, yBounds, zBounds) = Bounds3d.coordinates Gltf.convention meshBounds
    let Bounds xLow xHigh = xBounds
    let Bounds yLow yHigh = yBounds
    let Bounds zLow zHigh = zBounds
    List.singleton
      GltfMesh
        { frame = parentFrame
        , gltfMaterial = Gltf.pbrMaterial pbrMaterial
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
