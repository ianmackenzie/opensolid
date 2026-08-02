module OpenSolid.SurfaceFunction3D.Nondegenerate
  ( pointAt
  , pointOn
  , partialDerivatives
  , normalDirectionAt
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D.Nonzero qualified as Vector3D.Nonzero
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D.Nondegenerate qualified as VectorSurfaceFunction3D.Nondegenerate

pointAt :: UvPoint -> Nondegenerate (SurfaceFunction3D space) -> Point3D space
pointAt uvPoint (Nondegenerate function) = SurfaceFunction3D.pointAt uvPoint function

pointOn :: Nondegenerate (SurfaceFunction3D space) -> UvPoint -> Point3D space
pointOn function uvPoint = pointAt uvPoint function

partialDerivatives ::
  Nondegenerate (SurfaceFunction3D space) ->
  ( Nondegenerate (VectorSurfaceFunction3D Meters space)
  , Nondegenerate (VectorSurfaceFunction3D Meters space)
  )
partialDerivatives (Nondegenerate function) =
  Pair.map Nondegenerate (SurfaceFunction3D.partialDerivatives function)

normalDirectionAt ::
  Tolerance Meters =>
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  Direction3D space
normalDirectionAt uvPoint function = do
  let (fu, fv) = partialDerivatives function
  let fuDirection = VectorSurfaceFunction3D.Nondegenerate.direction fu uvPoint
  let fvDirection = VectorSurfaceFunction3D.Nondegenerate.direction fv uvPoint
  let crossProduct = fuDirection `cross` fvDirection
  Vector3D.Nonzero.direction (Nonzero crossProduct)
