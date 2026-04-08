module OpenSolid.Triangle3D
  ( Triangle3D (Triangle3D)
  , fromVertices
  , vertices
  , area
  )
where

import OpenSolid.Area (Area)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Vector3D qualified as Vector3D

-- | A triangle in 3D.
data Triangle3D space
  = -- | Construct a triangle from its three vertices.
    Triangle3D (Point3D space) (Point3D space) (Point3D space)

instance FFI (Triangle3D FFI.Space) where
  representation = FFI.classRepresentation "Triangle3D"

fromVertices :: (Point3D space, Point3D space, Point3D space) -> Triangle3D space
fromVertices (p1, p2, p3) = Triangle3D p1 p2 p3

-- | Get the vertices of a triangle as a tuple.
vertices :: Triangle3D space -> (Point3D space, Point3D space, Point3D space)
vertices (Triangle3D p1 p2 p3) = (p1, p2, p3)

-- | Compute the area of a triangle.
area :: Triangle3D space -> Area
area (Triangle3D p1 p2 p3) = 0.5 * Vector3D.magnitude ((p2 - p1) `cross` (p3 - p1))
