module OpenSolid.Plane3D (originPoint, normalDirection) where

import OpenSolid.Primitives (Direction3D, Plane3D, Point3D)

originPoint :: Plane3D global local -> Point3D global
normalDirection :: Plane3D global local -> Direction3D global
