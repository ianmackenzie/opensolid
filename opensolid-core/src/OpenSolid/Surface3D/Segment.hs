module OpenSolid.Surface3D.Segment
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

import Data.Coerce qualified
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import OpenSolid.VectorBounds3D (VectorBounds3D)

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
  , isMonotonic :: ~Bool
  }

instance Space.Coercion (Segment space1) (Segment space2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

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
isMonotonic = (.isMonotonic)

areDistinct :: Tolerance Meters => Segment space -> Segment space -> Bool
areDistinct segment1 segment2 = not (range segment1 ^ range segment2)

haveCrossingNormals :: Segment space -> Segment space -> Bool
haveCrossingNormals segment1 segment2 =
  DirectionBounds3D.areIndependent (normalDirectionRange segment1) (normalDirectionRange segment2)
