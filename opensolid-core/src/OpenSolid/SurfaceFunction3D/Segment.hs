module OpenSolid.SurfaceFunction3D.Segment
  ( Segment
  , range
  , partialDerivativeRanges
  , secondPartialDerivativeRanges
  , normalDirectionRange
  , isDegenerate
  , new
  , isMonotonic
  , areDistinct
  , haveCrossingNormals
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Degeneracy qualified as Degeneracy
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D.Nondegenerate qualified as SurfaceFunction3D.Nondegenerate
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D

data Segment space = Segment
  { range :: ~(Bounds3D space)
  , partialDerivativeRanges :: (VectorBounds3D Meters space, VectorBounds3D Meters space)
  , secondPartialDerivativeRanges ::
      ( VectorBounds3D Meters space
      , VectorBounds3D Meters space
      , VectorBounds3D Meters space
      )
  , normalDirectionRange :: ~(DirectionBounds3D space)
  , isDegenerate :: ~Bool
  }

range :: Segment space -> Bounds3D space
range = (.range)

partialDerivativeRanges ::
  Segment space ->
  (VectorBounds3D Meters space, VectorBounds3D Meters space)
partialDerivativeRanges = (.partialDerivativeRanges)

secondPartialDerivativeRanges ::
  Segment space ->
  ( VectorBounds3D Meters space
  , VectorBounds3D Meters space
  , VectorBounds3D Meters space
  )
secondPartialDerivativeRanges = (.secondPartialDerivativeRanges)

normalDirectionRange :: Segment space -> DirectionBounds3D space
normalDirectionRange = (.normalDirectionRange)

isDegenerate :: Segment space -> Bool
isDegenerate = (.isDegenerate)

isMonotonic :: Segment space -> Bool
isMonotonic segment = do
  let (duBounds, dvBounds) = partialDerivativeRanges segment
  VectorBounds3D.areIndependent duBounds dvBounds

areDistinct :: Tolerance Meters => Segment space -> Segment space -> Bool
areDistinct segment1 segment2 = not (range segment1 ^ range segment2)

haveCrossingNormals :: Segment space -> Segment space -> Bool
haveCrossingNormals segment1 segment2 =
  DirectionBounds3D.areIndependent (normalDirectionRange segment1) (normalDirectionRange segment2)

new :: Nondegenerate (SurfaceFunction3D space) -> UvBounds -> Segment space
new function uvRange = do
  let UvBounds (Interval uLow uHigh) (Interval vLow vHigh) = uvRange
  let isDegenerateLeft =
        uHigh <= Degeneracy.tStart && SurfaceFunction3D.Nondegenerate.degenerateLeft function
  let isDegenerateRight =
        uLow >= Degeneracy.tEnd && SurfaceFunction3D.Nondegenerate.degenerateRight function
  let isDegenerateBottom =
        vHigh <= Degeneracy.tStart && SurfaceFunction3D.Nondegenerate.degenerateBottom function
  let isDegenerateTop =
        vLow >= Degeneracy.tEnd && SurfaceFunction3D.Nondegenerate.degenerateTop function
  Segment
    { range = SurfaceFunction3D.Nondegenerate.range uvRange function
    , partialDerivativeRanges =
        SurfaceFunction3D.Nondegenerate.partialDerivativeRanges uvRange function
    , secondPartialDerivativeRanges =
        SurfaceFunction3D.Nondegenerate.secondPartialDerivativeRanges uvRange function
    , normalDirectionRange =
        SurfaceFunction3D.Nondegenerate.normalDirectionRange uvRange function
    , isDegenerate = isDegenerateLeft || isDegenerateRight || isDegenerateBottom || isDegenerateTop
    }
