module OpenSolid.Gltf
  ( convention
  , builder
  , writeBinary
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import OpenSolid.Array (Array)
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounded3d qualified as Bounded3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3d (Convention3d (Convention3d))
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Model3d (Model3d)
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.Orientation3d (Orientation3d)
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Resolution (Resolution)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.Vertex3d qualified as Vertex3d

xDirection :: Orientation3d space -> Direction3d space
xDirection = Orientation3d.leftwardDirection

yDirection :: Orientation3d space -> Direction3d space
yDirection = Orientation3d.upwardDirection

zDirection :: Orientation3d space -> Direction3d space
zDirection = Orientation3d.forwardDirection

convention :: Convention3d
convention = Convention3d xDirection yDirection zDirection

builder :: Resolution Meters -> Model3d space -> Builder
builder resolution model = do
  let meshes = gltfMeshes resolution model
  let sceneObject = Json.object [Json.field "nodes" $ Json.listOf Json.int [0 .. meshes.length - 1]]
  let unpaddedBufferBuilder = Binary.collect meshBuilder meshes
  let unpaddedBufferByteLength = Int.sumOf meshByteLength meshes
  let encodedMeshes = encodeMeshes 0 0 meshes
  let bufferObject = Json.object [Json.field "byteLength" $ Json.int unpaddedBufferByteLength]
  let assetObject =
        Json.object
          [ Json.field "version" $ Json.text "2.0"
          , Json.field "generator" $ Json.text "OpenSolid"
          ]
  let json =
        Json.object
          [ Json.field "asset" $ assetObject
          , Json.field "scene" $ Json.int 0
          , Json.field "scenes" $ Json.list [sceneObject]
          , Json.field "nodes" $ Json.listOf (.meshNode) encodedMeshes
          , Json.field "meshes" $ Json.listOf (.meshObject) encodedMeshes
          , Json.field "bufferViews" $ Json.list (List.collect (.bufferViews) encodedMeshes)
          , Json.field "accessors" $ Json.list (List.collect (.accessors) encodedMeshes)
          , Json.field "materials" $ Json.listOf (.materialObject) encodedMeshes
          , Json.field "buffers" $ Json.list [bufferObject]
          ]
  let unpaddedJsonBuilder = Json.toBinary json
  let unpaddedJsonByteString = Binary.bytes unpaddedJsonBuilder
  let unpaddedJsonByteLength = ByteString.length unpaddedJsonByteString
  let (paddedJsonBuilder, paddedJsonByteLength) =
        padWith ' ' (Builder.byteString unpaddedJsonByteString) unpaddedJsonByteLength
  let (paddedBufferBuilder, paddedBufferByteLength) =
        padWith '\0' unpaddedBufferBuilder unpaddedBufferByteLength
  let headerByteLength = 12
  let jsonChunkByteLength = 8 + paddedJsonByteLength
  let binaryChunkByteLength = 8 + paddedBufferByteLength
  let fileByteLength = headerByteLength + jsonChunkByteLength + binaryChunkByteLength
  Binary.concat
    [ Binary.uint32LE 0x46546C67 -- magic 'gltf' identifier
    , Binary.uint32LE 2 -- file format version
    , Binary.uint32LE fileByteLength
    , Binary.uint32LE paddedJsonByteLength
    , Binary.uint32LE 0x4E4F534A -- JSON chunk type
    , paddedJsonBuilder
    , Binary.uint32LE paddedBufferByteLength
    , Binary.uint32LE 0x004E4942 -- Binary buffer chunk type
    , paddedBufferBuilder
    ]

-- | Write a model to a binary glTF file with the given resolution.
writeBinary :: Text -> Model3d space -> Resolution Meters -> IO ()
writeBinary path model resolution = IO.writeBinary path (builder resolution model)

data GltfMesh where
  GltfMesh ::
    { gltfMaterial :: Json
    , numFaces :: Int
    , indices :: Builder
    , indicesByteLength :: Int
    , numVertices :: Int
    , vertices :: Builder
    , verticesByteLength :: Int
    , minPosition :: Json
    , maxPosition :: Json
    } ->
    GltfMesh

data EncodedMesh = EncodedMesh
  { bufferViews :: List Json
  , accessors :: List Json
  , meshObject :: Json
  , meshNode :: Json
  , materialObject :: Json
  }

gltfMeshes :: Resolution Meters -> Model3d space -> List GltfMesh
gltfMeshes resolution model = case model of
  Model3d.Body _ body_ -> do
    let mesh = Body3d.toMesh resolution body_
    let meshVertices = mesh.vertices
    let numVertices = meshVertices.length
    let meshFaceIndices = mesh.faceIndices
    let numFaces = meshFaceIndices.length
    let meshBounds = Bounded3d.bounds mesh
    let (xBounds, yBounds, zBounds) = Bounds3d.coordinates convention meshBounds
    let Bounds xLow xHigh = xBounds
    let Bounds yLow yHigh = yBounds
    let Bounds zLow zHigh = zBounds
    List.singleton
      GltfMesh
        { gltfMaterial = encodeMaterial model.pbrMaterial
        , numFaces
        , indices = faceIndicesBuilder meshFaceIndices
        , indicesByteLength = 3 * 4 * numFaces
        , numVertices
        , vertices = verticesBuilder meshVertices
        , verticesByteLength = 6 * 4 * numVertices
        , minPosition = Json.listOf (Json.float . Length.inMeters) [xLow, yLow, zLow]
        , maxPosition = Json.listOf (Json.float . Length.inMeters) [xHigh, yHigh, zHigh]
        }
  Model3d.Group _ children -> List.collect (gltfMeshes resolution) children

encodeMeshes :: Int -> Int -> List GltfMesh -> List EncodedMesh
encodeMeshes index offset meshes = case meshes of
  [] -> []
  mesh : rest -> do
    let indicesBufferViewIndex = 2 * index
    let verticesBufferViewIndex = 2 * index + 1
    let indicesAccessorIndex = 3 * index
    let positionAccessorIndex = 3 * index + 1
    let normalAccessorIndex = 3 * index + 2
    let indicesBufferView =
          Json.object
            [ Json.field "buffer" $ Json.int 0
            , Json.field "byteOffset" $ Json.int offset
            , Json.field "byteLength" $ Json.int mesh.indicesByteLength
            , Json.field "target" $ Json.int 34963 -- ELEMENT_ARRAY_BUFFER, for vertex indices
            ]
    let verticesBufferView =
          Json.object
            [ Json.field "buffer" $ Json.int 0
            , Json.field "byteOffset" $ Json.int (offset + mesh.indicesByteLength)
            , Json.field "byteLength" $ Json.int mesh.verticesByteLength
            , Json.field "byteStride" $ Json.int 24
            , Json.field "target" $ Json.int 34962 -- ARRAY_BUFFER, for vertex attributes
            ]
    let indicesAccessor =
          Json.object
            [ Json.field "bufferView" $ Json.int indicesBufferViewIndex
            , Json.field "byteOffset" $ Json.int 0
            , Json.field "type" $ Json.text "SCALAR"
            , Json.field "componentType" $ Json.int 5125 -- UNSIGNED_INT
            , Json.field "count" $ Json.int (3 * mesh.numFaces)
            ]
    let positionAccessor =
          Json.object
            [ Json.field "bufferView" $ Json.int verticesBufferViewIndex
            , Json.field "byteOffset" $ Json.int 0
            , Json.field "type" $ Json.text "VEC3"
            , Json.field "componentType" $ Json.int 5126 -- FLOAT
            , Json.field "count" $ Json.int mesh.numVertices
            , Json.field "min" $ mesh.minPosition
            , Json.field "max" $ mesh.maxPosition
            ]
    let normalAccessor =
          Json.object
            [ Json.field "bufferView" $ Json.int verticesBufferViewIndex
            , Json.field "byteOffset" $ Json.int 12
            , Json.field "type" $ Json.text "VEC3"
            , Json.field "componentType" $ Json.int 5126 -- FLOAT
            , Json.field "count" $ Json.int mesh.numVertices
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
    let meshNode = Json.object [Json.field "mesh" $ Json.int index]
    let encodedMesh =
          EncodedMesh
            { bufferViews = [indicesBufferView, verticesBufferView]
            , accessors = [indicesAccessor, positionAccessor, normalAccessor]
            , meshObject
            , meshNode
            , materialObject = mesh.gltfMaterial
            }
    let updatedOffset = offset + mesh.indicesByteLength + mesh.verticesByteLength
    encodedMesh : encodeMeshes (index + 1) updatedOffset rest

meshBuilder :: GltfMesh -> Builder
meshBuilder mesh = mesh.indices <> mesh.vertices

meshByteLength :: GltfMesh -> Int
meshByteLength mesh = mesh.indicesByteLength + mesh.verticesByteLength

encodeMaterial :: Maybe PbrMaterial -> Json
encodeMaterial maybeMaterial = do
  let material = maybeMaterial |> Maybe.withDefault (PbrMaterial.aluminum (#roughness 0.2))
  let (r, g, b) = Color.rgbFloatComponents material.baseColor
  Json.object
    [ Json.field "pbrMetallicRoughness" do
        Json.object
          [ Json.field "baseColorFactor" $ Json.listOf Json.float [r, g, b, 1.0]
          , Json.field "metallicFactor" $ Json.float material.metallic
          , Json.field "roughnessFactor" $ Json.float material.roughness
          ]
    ]

faceIndicesBuilder :: List (Int, Int, Int) -> Builder
faceIndicesBuilder = Binary.collect do
  \(i, j, k) -> Binary.uint32LE i <> Binary.uint32LE j <> Binary.uint32LE k

verticesBuilder :: Vertex3d.HasNormal vertex (space @ Meters) => Array vertex -> Builder
verticesBuilder = Binary.collect vertexBuilder

vertexBuilder :: Vertex3d.HasNormal vertex (space @ Meters) => vertex -> Builder
vertexBuilder vertex = do
  let (px, py, pz) = Point3d.coordinates convention (Vertex3d.position vertex)
  let (nx, ny, nz) = Vector3d.components convention (Vertex3d.normal vertex)
  Binary.concat
    [ Binary.float32LE (Length.inMeters px)
    , Binary.float32LE (Length.inMeters py)
    , Binary.float32LE (Length.inMeters pz)
    , Binary.float32LE nx
    , Binary.float32LE ny
    , Binary.float32LE nz
    ]

paddedByteLength :: Int -> Int
paddedByteLength unpaddedLength = do
  let excess = unpaddedLength % 4
  if excess == 0 then unpaddedLength else unpaddedLength + (4 - excess)

padWith :: Char -> Builder -> Int -> (Builder, Int)
padWith char unpaddedBuilder unpaddedLength = do
  let paddedLength = paddedByteLength unpaddedLength
  let padding = Builder.string7 (List.repeat (paddedLength - unpaddedLength) char)
  (unpaddedBuilder <> padding, paddedLength)
