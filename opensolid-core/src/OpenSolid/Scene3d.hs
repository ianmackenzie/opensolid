module OpenSolid.Scene3d
  ( Entity
  , mesh
  , group
  , transformBy
  , placeIn
  , relativeTo
  , toGlb
  , writeGlb
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Monoid qualified
import OpenSolid.Array qualified as Array
import OpenSolid.Bounds3d (Bounds3d (Bounds3d))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Int qualified as Int
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import OpenSolid.Scene3d.Gltf qualified as Gltf
import OpenSolid.Scene3d.Material (Material)
import OpenSolid.Scene3d.Material qualified as Material
import OpenSolid.Text qualified as Text
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units (Meters)
import OpenSolid.Vertex3d (Vertex3d)

data Entity space where
  Mesh ::
    Vertex3d vertex (space @ Meters) =>
    Mesh vertex ->
    Material vertex ->
    Entity space
  Group ::
    List (Entity space) ->
    Entity space
  Placed ::
    Frame3d (global @ Meters) (Defines local) ->
    Entity local ->
    Entity global

data Ground

mesh :: Vertex3d vertex (space @ Meters) => Mesh vertex -> Material vertex -> Entity space
mesh = Mesh

group :: List (Entity space) -> Entity space
group = Group

transformBy :: Transform3d.Rigid (space @ Meters) -> Entity space -> Entity space
transformBy transform (Placed frame entity) = Placed (Frame3d.transformBy transform frame) entity
transformBy transform entity = Placed (Frame3d.transformBy transform Frame3d.xyz) entity

placeIn :: Frame3d (global @ Meters) (Defines local) -> Entity local -> Entity global
placeIn frame (Placed entityFrame entity) = Placed (Frame3d.placeIn frame entityFrame) entity
placeIn frame entity = Placed frame entity

relativeTo :: Frame3d (global @ Meters) (Defines local) -> Entity global -> Entity local
relativeTo frame = placeIn (Frame3d.inverse frame)

toGlb :: Plane3d (space @ Meters) (Defines Ground) -> List (Entity space) -> Builder
toGlb groundPlane givenEntities = do
  let globalFrame = Frame3d.fromZxPlane groundPlane
  let entities = List.map (relativeTo globalFrame) givenEntities
  let meshes = gltfMeshes Frame3d.xyz (group entities)
  let sceneObject = Json.object [Json.field "nodes" $ Json.listOf Json.int [0 .. List.length meshes - 1]]
  let bufferBuilder = Data.Monoid.mconcat (List.map meshBuilder meshes)
  let bufferByteLength = Int.sumOf meshByteLength meshes
  let encodedMeshes = encodeMeshes 0 0 meshes
  let bufferObject = Json.object [Json.field "byteLength" $ Json.int bufferByteLength]
  Gltf.builder
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

writeGlb :: Text -> Plane3d (space @ Meters) (Defines Ground) -> List (Entity space) -> IO ()
writeGlb path groundPlane givenEntities =
  Builder.writeFile (Text.unpack path) (toGlb groundPlane givenEntities)

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
    , material
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
              , materialObject = material
              }
      encodedMesh : encodeMeshes (index + 1) (offset + indicesByteLength + verticesByteLength) rest

data GltfMesh space where
  GltfMesh ::
    { frame :: Frame3d (space @ Meters) defines
    , material :: Json
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
  forall global space.
  Frame3d (global @ Meters) (Defines space) ->
  Entity space ->
  List (GltfMesh global)
gltfMeshes parentFrame entity = case entity of
  Mesh smoothMesh Material.Pbr{baseColor, roughness, metallic} -> do
    let vertices = Mesh.vertices smoothMesh
    let numVertices = Array.length vertices
    let faceIndices = Mesh.faceIndices smoothMesh
    let numFaces = List.length faceIndices
    let meshBounds :: Bounds3d (space @ Meters) = Bounds3d.bounds smoothMesh
    let Bounds3d xRange yRange zRange = meshBounds
    let Range xLow xHigh = xRange
    let Range yLow yHigh = yRange
    let Range zLow zHigh = zRange
    List.singleton
      GltfMesh
        { frame = parentFrame
        , material = Gltf.pbrMaterial baseColor roughness metallic
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
meshBuilder GltfMesh{indices, vertices} = Data.Monoid.mappend indices vertices

meshByteLength :: GltfMesh space -> Int
meshByteLength GltfMesh{indicesByteLength, verticesByteLength} =
  indicesByteLength + verticesByteLength
