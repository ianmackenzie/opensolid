module OpenSolid.VectorSurfaceFunction3D.Nondegenerate (directionValue) where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

directionValue ::
  Nondegenerate (VectorSurfaceFunction3D units space) ->
  UvPoint ->
  Direction3D space
directionValue (Nondegenerate function) uvPoint = do
  let UvPoint uValue vValue = uvPoint
  Direction3D.unsafe $
    Tolerance.using Quantity.zero $
      Vector3D.normalize $
        if
          | uValue == 0.0 && VectorSurfaceFunction3D.singularU0 function ->
              VectorSurfaceFunction3D.derivativeValue U function uvPoint
          | uValue == 1.0 && VectorSurfaceFunction3D.singularU1 function ->
              negate (VectorSurfaceFunction3D.derivativeValue U function uvPoint)
          | vValue == 0.0 && VectorSurfaceFunction3D.singularV0 function ->
              VectorSurfaceFunction3D.derivativeValue V function uvPoint
          | vValue == 1.0 && VectorSurfaceFunction3D.singularV1 function ->
              negate (VectorSurfaceFunction3D.derivativeValue V function uvPoint)
          | otherwise ->
              VectorSurfaceFunction3D.value function uvPoint
