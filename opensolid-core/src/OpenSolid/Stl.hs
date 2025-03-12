module OpenSolid.Stl
  ( toText
  , toBinary
  , writeText
  , writeBinary
  )
where

import Data.ByteString.Builder qualified as Builder
import GHC.Float qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

toText :: Vertex3d vertex (space @ units) => (Qty units -> Float) -> Mesh vertex -> Text
toText units mesh =
  Text.multiline
    [ "solid"
    , Text.multiline (List.map (triangleText units) (Mesh.faceVertices mesh))
    , "endsolid"
    ]

toBinary :: Vertex3d vertex (space @ units) => (Qty units -> Float) -> Mesh vertex -> Builder
toBinary units mesh = do
  let header = Binary.concat (List.repeat 80 (Builder.word8 (fromIntegral 0)))
  let triangleCount = Builder.word32LE (fromIntegral (List.length (Mesh.faceIndices mesh)))
  let triangles = Binary.collect (triangleBuilder units) (Mesh.faceVertices mesh)
  Binary.concat [header, triangleCount, triangles]

writeText :: Vertex3d vertex (space @ units) => Text -> (Qty units -> Float) -> Mesh vertex -> IO ()
writeText path units mesh = IO.writeUtf8 path (toText units mesh)

writeBinary :: Vertex3d vertex (space @ units) => Text -> (Qty units -> Float) -> Mesh vertex -> IO ()
writeBinary path units mesh = IO.writeBinary path (toBinary units mesh)

floatBuilder :: Float -> Builder
floatBuilder value = Builder.floatLE (GHC.Float.double2Float (Float.toDouble value))

vectorBuilder :: Vector3d (space @ Unitless) -> Builder
vectorBuilder (Vector3d x y z) = Binary.concat [floatBuilder x, floatBuilder y, floatBuilder z]

pointBuilder :: (Qty units -> Float) -> Point3d (space @ units) -> Builder
pointBuilder units (Point3d x y z) =
  Binary.concat [floatBuilder (units x), floatBuilder (units y), floatBuilder (units z)]

triangleBuilder ::
  Vertex3d vertex (space @ units) =>
  (Qty units -> Float) ->
  (vertex, vertex, vertex) ->
  Builder
triangleBuilder units (v0, v1, v2) = do
  let p0 = Vertex3d.position v0
  let p1 = Vertex3d.position v1
  let p2 = Vertex3d.position v2
  let normal = Vector3d.normalize ((p1 - p0) `cross'` (p2 - p0))
  Binary.concat
    [ vectorBuilder normal
    , pointBuilder units p0
    , pointBuilder units p1
    , pointBuilder units p2
    , Builder.word16LE (fromIntegral 0)
    ]

triangleText ::
  Vertex3d vertex (space @ units) =>
  (Qty units -> Float) ->
  (vertex, vertex, vertex) ->
  Text
triangleText units (v0, v1, v2) = do
  let p0 = Vertex3d.position v0
  let p1 = Vertex3d.position v1
  let p2 = Vertex3d.position v2
  let crossProduct = (p1 - p0) `cross'` (p2 - p0)
  let Vector3d nx ny nz = Vector3d.normalize crossProduct
  Text.multiline
    [ "facet normal " + Text.float nx + " " + Text.float ny + " " + Text.float nz
    , "    outer loop"
    , "        " + vertex units p0
    , "        " + vertex units p1
    , "        " + vertex units p2
    , "    endloop"
    , "endfacet"
    ]

vertex :: (Qty units -> Float) -> Point3d (space @ units) -> Text
vertex units (Point3d px py pz) =
  "vertex " + Text.float (units px) + " " + Text.float (units py) + " " + Text.float (units pz)
