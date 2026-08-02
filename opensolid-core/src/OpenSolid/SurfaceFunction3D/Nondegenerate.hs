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
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.Vector3D.Nonzero qualified as Vector3D.Nonzero
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

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
normalDirectionAt uvPoint (Nondegenerate function) = do
  let UvPoint uValue vValue = uvPoint
  let (fu, fv) = SurfaceFunction3D.partialDerivativesAt uvPoint function
  let (fuu, fuv, fvv) = SurfaceFunction3D.secondPartialDerivativesAt uvPoint function
  let n = fu `cross` fv
  let nu = fuu `cross` fv + fu `cross` fuv
  let nv = fuv `cross` fv + fu `cross` fvv
  Vector3D.Nonzero.direction . Nonzero $
    if
      | uValue == 0.0 && SurfaceFunction3D.degenerateLeft function -> nu
      | uValue == 1.0 && SurfaceFunction3D.degenerateRight function -> -nu
      | vValue == 0.0 && SurfaceFunction3D.degenerateBottom function -> nv
      | vValue == 1.0 && SurfaceFunction3D.degenerateTop function -> -nv
      | otherwise -> n
