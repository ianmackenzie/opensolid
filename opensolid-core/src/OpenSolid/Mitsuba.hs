module OpenSolid.Mitsuba
  ( convention
  , toBinary
  , writeBinary
  )
where

import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString.Builder qualified as Builder
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Convention3d (Convention3d (Convention3d))
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh (Mesh)
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Units (Meters)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

{-| The Mitsuba world coordinate convention (Y up, Z backward).

Source: https://github.com/mitsuba-renderer/mitsuba-blender/discussions/47#discussioncomment-3847744
-}
convention :: Convention3d
convention =
  Convention3d
    Orientation3d.rightwardDirection
    Orientation3d.upwardDirection
    Orientation3d.backwardDirection

toBinary :: Vertex3d.HasNormal vertex (space @ Meters) => Mesh vertex -> Builder
toBinary mesh = do
  Binary.concat
    [ Binary.uint16LE 0x041C -- File format identifier
    , Binary.uint16LE 0x0004 -- Version identifier
    , Builder.lazyByteString (Zlib.compress (Builder.toLazyByteString (content mesh)))
    , Binary.uint64LE 0 -- Index of first (and only) mesh
    , Binary.uint32LE 1 -- Total number of meshes
    ]

content :: Vertex3d.HasNormal vertex (space @ Meters) => Mesh vertex -> Builder
content mesh =
  Binary.concat
    [ Binary.uint32LE 0x2001 -- Flags: per-vertex normals, double precision
    , Binary.uint8 0 -- No name, so just the null termination byte for an empty UTF-8 string
    , Binary.uint64LE mesh.numVertices
    , Binary.uint64LE mesh.numFaces
    , Binary.collect position mesh.vertices
    , Binary.collect normal mesh.vertices
    , Binary.collect faceIndices mesh.faceIndices
    ]

position :: Vertex3d vertex (space @ Meters) => vertex -> Builder
position vertex = do
  let (px, py, pz) = Point3d.coordinates convention (Vertex3d.position vertex)
  Binary.concat
    [ Binary.float64LE (Length.inMeters px)
    , Binary.float64LE (Length.inMeters py)
    , Binary.float64LE (Length.inMeters pz)
    ]

normal :: Vertex3d.HasNormal vertex (space @ Meters) => vertex -> Builder
normal vertex = do
  let (nx, ny, nz) = Vector3d.components convention (Vertex3d.normal vertex)
  Binary.concat
    [ Binary.float64LE nx
    , Binary.float64LE ny
    , Binary.float64LE nz
    ]

faceIndices :: (Int, Int, Int) -> Builder
faceIndices (i, j, k) = Binary.uint32LE i <> Binary.uint32LE j <> Binary.uint32LE k

writeBinary :: Vertex3d.HasNormal vertex (space @ Meters) => Text -> Mesh vertex -> IO ()
writeBinary path mesh = IO.writeBinary path (toBinary mesh)
