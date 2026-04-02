module OpenSolid.Plane3D (originPoint, normalDirection) where

import OpenSolid.Primitives (Direction3D, Plane3D, Point3D)

originPoint :: Plane3D space -> Point3D space
normalDirection :: Plane3D space -> Direction3D space
