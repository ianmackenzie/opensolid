module OpenSolid.SurfaceFunction3D.Segment
  ( Segment (..)
  , range
  , partialDerivativeRanges
  , secondPartialDerivativeRanges
  , normalDirectionRange
  , isDegenerate
  , isMonotonic
  , areDistinct
  , haveCrossingNormals
  )
where

import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Prelude
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
