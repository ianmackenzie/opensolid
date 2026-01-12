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
import OpenSolid.Convention3D (Convention3D)
import OpenSolid.IO qualified as IO
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

toText :: Convention3D -> (Length -> Number) -> Mesh (Point3D space) -> Text
toText convention units mesh =
  Text.multiline
    [ "solid"
    , Text.multiline (List.map (triangleText convention units) (Mesh.faceVertices mesh))
    , "endsolid"
    ]

toBinary :: Convention3D -> (Length -> Number) -> Mesh (Point3D space) -> Builder
toBinary convention units mesh = do
  let emptyHeaderBytes = List.replicate 80 (Binary.uint8 0)
  let header = Binary.concat emptyHeaderBytes
  let triangleCount = Binary.uint32LE (Mesh.numFaces mesh)
  let triangles = Binary.combine (triangleBuilder convention units) (Mesh.faceVertices mesh)
  Binary.concat [header, triangleCount, triangles]

writeText :: Text -> Convention3D -> (Length -> Number) -> Mesh (Point3D space) -> IO ()
writeText path convention units mesh = IO.writeUtf8 path (toText convention units mesh)

writeBinary :: Text -> Convention3D -> (Length -> Number) -> Mesh (Point3D space) -> IO ()
writeBinary path convention units mesh = IO.writeBinary path (toBinary convention units mesh)

numberBuilder :: Number -> Builder
numberBuilder (Quantity double) = Builder.floatLE (GHC.Float.double2Float double)

vectorBuilder :: Convention3D -> Vector3D Unitless space -> Builder
vectorBuilder convention vector = do
  let (x, y, z) = Vector3D.components convention vector
  numberBuilder x <> numberBuilder y <> numberBuilder z

pointBuilder :: Convention3D -> (Length -> Number) -> Point3D space -> Builder
pointBuilder convention units point = do
  let (x, y, z) = Point3D.coordinates convention point
  numberBuilder (units x) <> numberBuilder (units y) <> numberBuilder (units z)

triangleBuilder ::
  Convention3D ->
  (Length -> Number) ->
  (Point3D space, Point3D space, Point3D space) ->
  Builder
triangleBuilder convention units (p0, p1, p2) = do
  let normal = Vector3D.normalize ((p1 .-. p0) `cross_` (p2 .-. p0))
  Binary.concat
    [ vectorBuilder convention normal
    , pointBuilder convention units p0
    , pointBuilder convention units p1
    , pointBuilder convention units p2
    , Builder.word16LE 0
    ]

triangleText ::
  Convention3D ->
  (Length -> Number) ->
  (Point3D space, Point3D space, Point3D space) ->
  Text
triangleText convention units (p0, p1, p2) = do
  let crossProduct = (p1 .-. p0) `cross_` (p2 .-. p0)
  let (nx, ny, nz) = Vector3D.components convention (Vector3D.normalize crossProduct)
  Text.multiline
    [ "facet normal " <> Text.number nx <> " " <> Text.number ny <> " " <> Text.number nz
    , "    outer loop"
    , "        " <> pointText convention units p0
    , "        " <> pointText convention units p1
    , "        " <> pointText convention units p2
    , "    endloop"
    , "endfacet"
    ]

pointText :: Convention3D -> (Length -> Number) -> Point3D space -> Text
pointText convention units point = do
  let (px, py, pz) = Point3D.coordinates convention point
  Text.join " " ["vertex", Text.number (units px), Text.number (units py), Text.number (units pz)]
