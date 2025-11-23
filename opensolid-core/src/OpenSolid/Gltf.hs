{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Gltf
  ( convention
  , builder
  , writeBinary
  )
where

import Data.Array.Byte (ByteArray (ByteArray))
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Short (ShortByteString (ShortByteString))
import GHC.Exts qualified
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Body3d qualified as Body3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Color qualified as Color
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Model3d (Model3d)
import OpenSolid.Model3d qualified as Model3d
import OpenSolid.PbrMaterial (PbrMaterial)
import OpenSolid.PbrMaterial qualified as PbrMaterial
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Resolution (Resolution)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.Vertex3d qualified as Vertex3d

convention :: Convention3d
convention = Convention3d.yUp

builder :: Resolution Meters -> Model3d space -> Builder
builder resolution model = do
  let meshes = Model3d.inspect (gltfMeshes resolution) model
  let sceneObject =
        Json.object [Json.field "nodes" $ Json.listOf Json.int [0 .. List.length meshes - 1]]
  let unpaddedBufferBuilder = Binary.combine meshBuilder meshes
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
          , Json.field "bufferViews" $ Json.list (List.combine (.bufferViews) encodedMeshes)
          , Json.field "accessors" $ Json.list (List.combine (.accessors) encodedMeshes)
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

gltfMeshes :: Model3d.Traversal => Resolution Meters -> Model3d space -> List GltfMesh
gltfMeshes resolution model = case model of
  Model3d.Body body -> do
    let mesh = Body3d.toMesh resolution body
    case Array.toList (Mesh.vertices mesh) of
      NonEmpty meshVertices -> do
        let numVertices = Array.length (Mesh.vertices mesh)
        let meshFaceIndices = Mesh.faceIndices mesh
        let numFaces = List.length meshFaceIndices
        let meshBounds = Bounds3d.hullN meshVertices
        let (xBounds, yBounds, zBounds) = Bounds3d.coordinates convention meshBounds
        let Bounds xLow xHigh = xBounds
        let Bounds yLow yHigh = yBounds
        let Bounds zLow zHigh = zBounds
        let pbrMaterial = Model3d.traversal.currentPbrMaterial
        List.singleton
          GltfMesh
            { gltfMaterial = encodeMaterial pbrMaterial
            , numFaces
            , indices = faceIndicesBuilder meshFaceIndices
            , indicesByteLength = 3 * 4 * numFaces
            , numVertices
            , vertices = verticesBuilder (Mesh.vertices mesh)
            , verticesByteLength = 6 * 4 * numVertices
            , minPosition = Json.listOf (Json.number . Length.inMeters) [xLow, yLow, zLow]
            , maxPosition = Json.listOf (Json.number . Length.inMeters) [xHigh, yHigh, zHigh]
            }
      [] -> []
  Model3d.Group children -> List.combine (gltfMeshes resolution) children

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

encodeMaterial :: PbrMaterial -> Json
encodeMaterial material = do
  let (r, g, b) = Color.toRgb1 material.baseColor
  Json.object
    [ Json.field "pbrMetallicRoughness" do
        Json.object
          [ Json.field "baseColorFactor" $ Json.listOf Json.number [r, g, b, 1]
          , Json.field "metallicFactor" $ Json.number material.metallic
          , Json.field "roughnessFactor" $ Json.number material.roughness
          ]
    ]

faceIndicesBuilder :: List (Int, Int, Int) -> Builder
faceIndicesBuilder = Binary.combine do
  \(i, j, k) -> Binary.uint32LE i <> Binary.uint32LE j <> Binary.uint32LE k

verticesBuilder :: Vertex3d.HasNormal vertex space Meters => Array vertex -> Builder
verticesBuilder = Binary.combine vertexBuilder

vertexBuilder :: Vertex3d.HasNormal vertex space Meters => vertex -> Builder
vertexBuilder vertex = GHC.Exts.runRW# \state0# -> do
  let !(# state1#, mutableByteArray# #) = GHC.Exts.newByteArray# 24# state0#
  let !(# px##, py##, pz## #) = Point3d.yUpCoordinates## (Vertex3d.position vertex)
  let !(# nx##, ny##, nz## #) = Vector3d.yUpComponents## (Vertex3d.normal vertex)
  let state2# = GHC.Exts.writeFloatArray# mutableByteArray# 0# (GHC.Exts.double2Float# px##) state1#
  let state3# = GHC.Exts.writeFloatArray# mutableByteArray# 1# (GHC.Exts.double2Float# py##) state2#
  let state4# = GHC.Exts.writeFloatArray# mutableByteArray# 2# (GHC.Exts.double2Float# pz##) state3#
  let state5# = GHC.Exts.writeFloatArray# mutableByteArray# 3# (GHC.Exts.double2Float# nx##) state4#
  let state6# = GHC.Exts.writeFloatArray# mutableByteArray# 4# (GHC.Exts.double2Float# ny##) state5#
  let state7# = GHC.Exts.writeFloatArray# mutableByteArray# 5# (GHC.Exts.double2Float# nz##) state6#
  let !(# _, byteArray# #) = GHC.Exts.unsafeFreezeByteArray# mutableByteArray# state7#
  Builder.shortByteString (ShortByteString (ByteArray byteArray#))

paddedByteLength :: Int -> Int
paddedByteLength unpaddedLength = do
  let excess = unpaddedLength `mod` 4
  if excess == 0 then unpaddedLength else unpaddedLength + (4 - excess)

padWith :: Char -> Builder -> Int -> (Builder, Int)
padWith char unpaddedBuilder unpaddedLength = do
  let paddedLength = paddedByteLength unpaddedLength
  let padding = Builder.string7 (List.replicate (paddedLength - unpaddedLength) char)
  (unpaddedBuilder <> padding, paddedLength)
