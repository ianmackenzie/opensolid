module OpenSolid.Stl (text) where

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

text :: Vertex3d vertex (space @ units) => (Qty units -> Float) -> Mesh vertex -> Text
text units mesh =
  Text.multiline
    [ "solid"
    , Text.multiline (List.map (facet units) (Mesh.faceVertices mesh))
    , "endsolid"
    ]

facet :: Vertex3d vertex (space @ units) => (Qty units -> Float) -> (vertex, vertex, vertex) -> Text
facet units (v0, v1, v2) = do
  let p0 = Vertex3d.position v0
  let p1 = Vertex3d.position v1
  let p2 = Vertex3d.position v2
  let crossProduct = (p1 - p0) .><. (p2 - p0)
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
