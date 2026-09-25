module OpenSolid.Surface3D.FindPoint (findPoint) where

import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Surface3D (Surface3D)
import OpenSolid.SurfacePoint3D (SurfacePoint3D)

findPoint :: Tolerance Meters => Point3D space -> Surface3D space -> List (SurfacePoint3D space)
