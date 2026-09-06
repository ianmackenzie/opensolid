module OpenSolid.SurfaceFunction3D.Nondegenerate
  ( pointAt
  , pointOn
  , range
  , partialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivatives
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
  , normalDirectionAt
  , normalDirectionRange
  , degenerateLeft
  , degenerateRight
  , degenerateBottom
  , degenerateTop
  , segment
  , bisectionTree
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Degeneracy qualified as Degeneracy
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceFunction3D.Segment (Segment (..))
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D.Nonzero qualified as Vector3D.Nonzero
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

pointAt :: UvPoint -> Nondegenerate (SurfaceFunction3D space) -> Point3D space
pointAt uvPoint (Nondegenerate function) = SurfaceFunction3D.pointAt uvPoint function

pointOn :: Nondegenerate (SurfaceFunction3D space) -> UvPoint -> Point3D space
pointOn function uvPoint = pointAt uvPoint function

range :: UvBounds -> Nondegenerate (SurfaceFunction3D space) -> Bounds3D space
range uvRange (Nondegenerate function) = SurfaceFunction3D.range uvRange function

partialDerivatives ::
  Nondegenerate (SurfaceFunction3D space) ->
  ( Nondegenerate (VectorSurfaceFunction3D Meters space)
  , Nondegenerate (VectorSurfaceFunction3D Meters space)
  )
partialDerivatives (Nondegenerate function) =
  Pair.map Nondegenerate (SurfaceFunction3D.partialDerivatives function)

partialDerivativesAt ::
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  (Vector3D Meters space, Vector3D Meters space)
partialDerivativesAt uvPoint (Nondegenerate function) =
  SurfaceFunction3D.partialDerivativesAt uvPoint function

partialDerivativeRanges ::
  UvBounds ->
  Nondegenerate (SurfaceFunction3D space) ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space)
partialDerivativeRanges uvRange (Nondegenerate function) =
  SurfaceFunction3D.partialDerivativeRanges uvRange function

secondPartialDerivatives ::
  Nondegenerate (SurfaceFunction3D space) ->
  ( VectorSurfaceFunction3D Meters space
  , VectorSurfaceFunction3D Meters space
  , VectorSurfaceFunction3D Meters space
  )
secondPartialDerivatives (Nondegenerate function) =
  SurfaceFunction3D.secondPartialDerivatives function

secondPartialDerivativesAt ::
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  (Vector3D Meters space, Vector3D Meters space, Vector3D Meters space)
secondPartialDerivativesAt uvPoint (Nondegenerate function) =
  SurfaceFunction3D.secondPartialDerivativesAt uvPoint function

secondPartialDerivativeRanges ::
  UvBounds ->
  Nondegenerate (SurfaceFunction3D space) ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space, VectorBounds3D Meters space)
secondPartialDerivativeRanges uvRange (Nondegenerate function) =
  SurfaceFunction3D.secondPartialDerivativeRanges uvRange function

normalDirectionAt ::
  UvPoint ->
  Nondegenerate (SurfaceFunction3D space) ->
  Direction3D space
normalDirectionAt uvPoint function = do
  let UvPoint uValue vValue = uvPoint
  let (fu, fv) = partialDerivativesAt uvPoint function
  let (fuu, fuv, fvv) = secondPartialDerivativesAt uvPoint function
  let n = fu `cross` fv
  let nu = fuu `cross` fv + fu `cross` fuv
  let nv = fuv `cross` fv + fu `cross` fvv
  Vector3D.Nonzero.direction . Nonzero $
    if
      | uValue == 0.0 && degenerateLeft function -> nu
      | uValue == 1.0 && degenerateRight function -> -nu
      | vValue == 0.0 && degenerateBottom function -> nv
      | vValue == 1.0 && degenerateTop function -> -nv
      | otherwise -> n

normalDirectionRange ::
  UvBounds ->
  Nondegenerate (SurfaceFunction3D space) ->
  DirectionBounds3D space
normalDirectionRange uvRange function = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  let (fuRange, fvRange) = partialDerivativeRanges uvRange function
  let (fuuRange, fuvRange, fvvRange) = secondPartialDerivativeRanges uvRange function
  let nRange = fuRange `cross` fvRange
  let nuRange = fuuRange `cross` fvRange + fuRange `cross` fuvRange
  let nvRange = fuvRange `cross` fvRange + fuRange `cross` fvvRange
  VectorBounds3D.direction $
    if
      | uLow == 0.0 && degenerateLeft function -> nuRange
      | uHigh == 1.0 && degenerateRight function -> -nuRange
      | vLow == 0.0 && degenerateBottom function -> nvRange
      | vHigh == 1.0 && degenerateTop function -> -nvRange
      | otherwise -> nRange

degenerateLeft :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateLeft (Nondegenerate function) = SurfaceFunction3D.degenerateLeft function

degenerateRight :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateRight (Nondegenerate function) = SurfaceFunction3D.degenerateRight function

degenerateBottom :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateBottom (Nondegenerate function) = SurfaceFunction3D.degenerateBottom function

degenerateTop :: Nondegenerate (SurfaceFunction3D space) -> Bool
degenerateTop (Nondegenerate function) = SurfaceFunction3D.degenerateTop function

segment :: UvBounds -> Nondegenerate (SurfaceFunction3D space) -> Segment space
segment uvRange function = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  let isDegenerateLeft = uHigh <= Degeneracy.tStart && degenerateLeft function
  let isDegenerateRight = uLow >= Degeneracy.tEnd && degenerateRight function
  let isDegenerateBottom = vHigh <= Degeneracy.tStart && degenerateBottom function
  let isDegenerateTop = vLow >= Degeneracy.tEnd && degenerateTop function
  Segment
    { range = range uvRange function
    , partialDerivativeRanges = partialDerivativeRanges uvRange function
    , secondPartialDerivativeRanges = secondPartialDerivativeRanges uvRange function
    , normalDirectionRange = normalDirectionRange uvRange function
    , isDegenerate = isDegenerateLeft || isDegenerateRight || isDegenerateBottom || isDegenerateTop
    }

bisectionTree :: Nondegenerate (SurfaceFunction3D space) -> SurfaceFunction3D.BisectionTree space
bisectionTree = SurfaceFunction3D.bisectionTree
