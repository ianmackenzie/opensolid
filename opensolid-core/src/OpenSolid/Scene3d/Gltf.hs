module OpenSolid.Scene3d.Gltf
  ( matrixField
  , pbrMaterial
  , uint32
  , faceIndices
  , pointsAndNormals
  , builder
  , toByteString
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Monoid qualified
import GHC.Float qualified
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Direction3d (Direction3d (Direction3d))
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Json (Json)
import OpenSolid.Json qualified as Json
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty))
import OpenSolid.Units (Meters)
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vertex3d qualified as Vertex3d

matrixComponents :: Frame3d (space @ Meters) defines -> Maybe Json
matrixComponents frame =
  if frame == Frame3d.xyz
    then Nothing
    else do
      let Direction3d ix iy iz = Frame3d.xDirection frame
      let Direction3d jx jy jz = Frame3d.yDirection frame
      let Direction3d kx ky kz = Frame3d.zDirection frame
      let Point3d x0 y0 z0 = Frame3d.originPoint frame
      let tx = Length.inMeters x0
      let ty = Length.inMeters y0
      let tz = Length.inMeters z0
      let components = [ix, iy, iz, 0.0, jx, jy, jz, 0.0, kx, ky, kz, 0.0, tx, ty, tz, 1.0]
      Just (Json.listOf Json.float components)

matrixField :: Frame3d (space @ Meters) defines -> Json.Field
matrixField frame = Json.optional "matrix" $ matrixComponents frame

pbrMaterial :: Color -> Float -> Float -> Json
pbrMaterial baseColor roughness metallic = do
  let (r, g, b) = Color.components baseColor
  Json.object
    [ Json.field "pbrMetallicRoughness" $
        Json.object
          [ Json.field "baseColorFactor" $ Json.listOf Json.float [r, g, b, 1.0]
          , Json.field "roughnessFactor" $ Json.float roughness
          , Json.field "metallicFactor" $ Json.float metallic
          ]
    ]

uint32 :: Int -> Builder
uint32 value = Builder.word32LE (fromIntegral value)

faceIndices :: List (Int, Int, Int) -> Builder
faceIndices indices = Data.Monoid.mconcat (List.map faceIndicesBuilder indices)

pointsAndNormals :: Vertex3d.HasNormal vertex (space @ Meters) => Array vertex -> Builder
pointsAndNormals vertices = do
  let addVertex accumulated vertex = Data.Monoid.mappend accumulated (pointNormalBuilder vertex)
  Array.foldl addVertex Data.Monoid.mempty vertices

faceIndicesBuilder :: (Int, Int, Int) -> Builder
faceIndicesBuilder (i, j, k) =
  Data.Monoid.mconcat
    [ Builder.word32LE (fromIntegral i)
    , Builder.word32LE (fromIntegral j)
    , Builder.word32LE (fromIntegral k)
    ]

pointNormalBuilder :: Vertex3d.HasNormal vertex (space @ Meters) => vertex -> Builder
pointNormalBuilder vertex = do
  let Point3d (Qty px) (Qty py) (Qty pz) = Vertex3d.position vertex
  let Vector3d (Qty nx) (Qty ny) (Qty nz) = Vertex3d.normal vertex
  Data.Monoid.mconcat
    [ Builder.floatLE (GHC.Float.double2Float px)
    , Builder.floatLE (GHC.Float.double2Float py)
    , Builder.floatLE (GHC.Float.double2Float pz)
    , Builder.floatLE (GHC.Float.double2Float nx)
    , Builder.floatLE (GHC.Float.double2Float ny)
    , Builder.floatLE (GHC.Float.double2Float nz)
    ]

builder :: List Json.Field -> Int -> Builder -> Builder
builder givenFields unpaddedBufferByteLength unpaddedBufferBuilder = do
  let assetObject =
        Json.object
          [ Json.field "version" $ Json.text "2.0"
          , Json.field "generator" $ Json.text "OpenSolid"
          ]
  let json = Json.object (Json.field "asset" assetObject : givenFields)
  let unpaddedJsonByteString = Json.encode json
  let unpaddedJsonByteLength = ByteString.length unpaddedJsonByteString
  let unpaddedJsonBuilder = Builder.byteString unpaddedJsonByteString
  let (paddedJsonBuilder, paddedJsonByteLength) =
        padWith ' ' unpaddedJsonBuilder unpaddedJsonByteLength
  let (paddedBufferBuilder, paddedBufferByteLength) =
        padWith '\0' unpaddedBufferBuilder unpaddedBufferByteLength
  let headerByteLength = 12
  let jsonChunkByteLength = 8 + paddedJsonByteLength
  let binaryChunkByteLength = 8 + paddedBufferByteLength
  let fileByteLength = headerByteLength + jsonChunkByteLength + binaryChunkByteLength
  Data.Monoid.mconcat
    [ uint32 0x46546C67 -- magic 'gltf' identifier
    , uint32 2 -- file format version
    , uint32 fileByteLength
    , uint32 paddedJsonByteLength
    , uint32 0x4E4F534A -- JSON chunk type
    , paddedJsonBuilder
    , uint32 paddedBufferByteLength
    , uint32 0x004E4942 -- Binary buffer chunk type
    , paddedBufferBuilder
    ]

toByteString :: List Json.Field -> Int -> Builder -> ByteString
toByteString givenFields bufferByteLength bufferBuilder =
  ByteString.toStrict $
    Builder.toLazyByteString $
      builder givenFields bufferByteLength bufferBuilder

paddedByteLength :: Int -> Int
paddedByteLength unpaddedLength = do
  let excess = unpaddedLength % 4
  if excess == 0 then unpaddedLength else unpaddedLength + (4 - excess)

padWith :: Char -> Builder -> Int -> (Builder, Int)
padWith char unpaddedBuilder unpaddedLength = do
  let paddedLength = paddedByteLength unpaddedLength
  let padding = Builder.string7 (List.repeat (paddedLength - unpaddedLength) char)
  (Data.Monoid.mappend unpaddedBuilder padding, paddedLength)
