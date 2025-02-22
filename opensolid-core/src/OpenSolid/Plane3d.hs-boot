module OpenSolid.Plane3d (through, normalDirection) where

import OpenSolid.Prelude
import OpenSolid.Primitives (Direction3d, Plane3d, Point3d)

through :: Point3d (space @ units) -> Direction3d space -> Plane3d (space @ units) defines
normalDirection :: Plane3d (space @ units) defines -> Direction3d space
