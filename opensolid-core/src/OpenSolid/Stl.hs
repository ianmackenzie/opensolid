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
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

toText ::
  Vertex3d vertex space units =>
  Convention3d ->
  (Quantity units -> Number) ->
  Mesh vertex ->
  Text
toText convention units mesh =
  Text.multiline
    [ "solid"
    , Text.multiline (List.map (triangleText convention units) (Mesh.faceVertices mesh))
    , "endsolid"
    ]

toBinary ::
  Vertex3d vertex space units =>
  Convention3d ->
  (Quantity units -> Number) ->
  Mesh vertex ->
  Builder
toBinary convention units mesh = do
  let emptyHeaderBytes = List.replicate 80 (Binary.uint8 0)
  let header = Binary.concat emptyHeaderBytes
  let triangleCount = Binary.uint32LE (Mesh.numFaces mesh)
  let triangles = Binary.combine (triangleBuilder convention units) (Mesh.faceVertices mesh)
  Binary.concat [header, triangleCount, triangles]

writeText ::
  Vertex3d vertex space units =>
  Text ->
  Convention3d ->
  (Quantity units -> Number) ->
  Mesh vertex ->
  IO ()
writeText path convention units mesh = IO.writeUtf8 path (toText convention units mesh)

writeBinary ::
  Vertex3d vertex space units =>
  Text ->
  Convention3d ->
  (Quantity units -> Number) ->
  Mesh vertex ->
  IO ()
writeBinary path convention units mesh = IO.writeBinary path (toBinary convention units mesh)

numberBuilder :: Number -> Builder
numberBuilder (Quantity double) = Builder.floatLE (GHC.Float.double2Float double)

vectorBuilder :: Convention3d -> Vector3d space Unitless -> Builder
vectorBuilder convention vector = do
  let (x, y, z) = Vector3d.components convention vector
  numberBuilder x <> numberBuilder y <> numberBuilder z

pointBuilder :: Convention3d -> (Quantity units -> Number) -> Point3d space units -> Builder
pointBuilder convention units point = do
  let (x, y, z) = Point3d.coordinates convention point
  numberBuilder (units x) <> numberBuilder (units y) <> numberBuilder (units z)

triangleBuilder ::
  Vertex3d vertex space units =>
  Convention3d ->
  (Quantity units -> Number) ->
  (vertex, vertex, vertex) ->
  Builder
triangleBuilder convention units (v0, v1, v2) = do
  let p0 = Vertex3d.position v0
  let p1 = Vertex3d.position v1
  let p2 = Vertex3d.position v2
  let normal = Vector3d.normalize ((p1 .-. p0) `cross_` (p2 .-. p0))
  Binary.concat
    [ vectorBuilder convention normal
    , pointBuilder convention units p0
    , pointBuilder convention units p1
    , pointBuilder convention units p2
    , Builder.word16LE 0
    ]

triangleText ::
  Vertex3d vertex space units =>
  Convention3d ->
  (Quantity units -> Number) ->
  (vertex, vertex, vertex) ->
  Text
triangleText convention units (v0, v1, v2) = do
  let p0 = Vertex3d.position v0
  let p1 = Vertex3d.position v1
  let p2 = Vertex3d.position v2
  let crossProduct = (p1 .-. p0) `cross_` (p2 .-. p0)
  let (nx, ny, nz) = Vector3d.components convention (Vector3d.normalize crossProduct)
  Text.multiline
    [ "facet normal " <> Text.number nx <> " " <> Text.number ny <> " " <> Text.number nz
    , "    outer loop"
    , "        " <> pointText convention units p0
    , "        " <> pointText convention units p1
    , "        " <> pointText convention units p2
    , "    endloop"
    , "endfacet"
    ]

pointText :: Convention3d -> (Quantity units -> Number) -> Point3d space units -> Text
pointText convention units point = do
  let (px, py, pz) = Point3d.coordinates convention point
  Text.join " " ["vertex", Text.number (units px), Text.number (units py), Text.number (units pz)]
