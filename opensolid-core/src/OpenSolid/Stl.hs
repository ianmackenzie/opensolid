module OpenSolid.Stl (text) where

import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Units (Meters)
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d

text :: (Length -> Float) -> Mesh (Point3d (space @ Meters)) -> Text
text units mesh =
  Text.multiline
    [ "solid"
    , Text.multiline (List.map (facet units) (Mesh.faceVertices mesh))
    , "endsolid"
    ]

facet ::
  (Length -> Float) ->
  (Point3d (space @ Meters), Point3d (space @ Meters), Point3d (space @ Meters)) ->
  Text
facet units (p0, p1, p2) = do
  let crossProduct = (p1 - p0) >< (p2 - p0)
  let Vector3d nx ny nz
        | crossProduct == Vector3d.zero = Vector3d.zero
        | otherwise = crossProduct / Vector3d.magnitude crossProduct
  Text.multiline
    [ "facet normal " + Text.float nx + " " + Text.float ny + " " + Text.float nz
    , "    outer loop"
    , "        " + vertex units p0
    , "        " + vertex units p1
    , "        " + vertex units p2
    , "    endloop"
    , "endfacet"
    ]

vertex :: (Length -> Float) -> Point3d (space @ Meters) -> Text
vertex units (Point3d px py pz) =
  "vertex " + Text.float (units px) + " " + Text.float (units py) + " " + Text.float (units pz)
