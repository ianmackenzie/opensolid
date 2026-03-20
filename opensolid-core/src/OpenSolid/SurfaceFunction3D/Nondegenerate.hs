module OpenSolid.SurfaceFunction3D.Nondegenerate
  ( point
  , derivative
  , normalDirectionValue
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D.Nondegenerate qualified as VectorSurfaceFunction3D.Nondegenerate

point ::
  Nondegenerate (SurfaceFunction3D space) ->
  UvPoint ->
  Point3D space
point (Nondegenerate function) uvPoint = SurfaceFunction3D.point function uvPoint

derivative ::
  SurfaceParameter ->
  Nondegenerate (SurfaceFunction3D space) ->
  Nondegenerate (VectorSurfaceFunction3D Meters space)
derivative parameter (Nondegenerate function) =
  Nondegenerate (SurfaceFunction3D.derivative parameter function)

normalDirectionValue ::
  Nondegenerate (SurfaceFunction3D space) ->
  UvPoint ->
  Direction3D space
normalDirectionValue function uvPoint = do
  let du = derivative U function
  let dv = derivative V function
  let duDirection = VectorSurfaceFunction3D.Nondegenerate.directionValue du uvPoint
  let dvDirection = VectorSurfaceFunction3D.Nondegenerate.directionValue dv uvPoint
  Direction3D.unsafe $
    Tolerance.using Quantity.zero $
      Vector3D.normalize (duDirection `cross` dvDirection)
