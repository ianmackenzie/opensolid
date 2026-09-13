module OpenSolid.Surface3D.Nondegenerate.FindPoint (findPoint) where

import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.SurfacePoint3D (SurfacePoint3D)

findPoint ::
  Tolerance Meters =>
  Point3D space ->
  Nondegenerate (Surface3D space) ->
  List (SurfacePoint3D space)
