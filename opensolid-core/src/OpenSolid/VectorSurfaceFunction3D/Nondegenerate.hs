module OpenSolid.VectorSurfaceFunction3D.Nondegenerate (directionAt) where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.Vector3D.Nonzero qualified as Vector3D.Nonzero
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

directionAt ::
  Tolerance units =>
  UvPoint ->
  Nondegenerate (VectorSurfaceFunction3D units space) ->
  Direction3D space
directionAt uvPoint (Nondegenerate function) = do
  let UvPoint uValue vValue = uvPoint
  let fValue = VectorSurfaceFunction3D.valueAt uvPoint function
  let (fuValue, fvValue) = VectorSurfaceFunction3D.partialDerivativesAt uvPoint function
  Vector3D.Nonzero.direction . Nonzero $
    if
      | uValue == 0.0 && VectorSurfaceFunction3D.degenerateLeft function -> fuValue
      | uValue == 1.0 && VectorSurfaceFunction3D.degenerateRight function -> -fuValue
      | vValue == 0.0 && VectorSurfaceFunction3D.degenerateBottom function -> fvValue
      | vValue == 1.0 && VectorSurfaceFunction3D.degenerateTop function -> -fvValue
      | otherwise -> fValue
