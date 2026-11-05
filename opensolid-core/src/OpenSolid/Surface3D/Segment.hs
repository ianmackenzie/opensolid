module OpenSolid.Surface3D.Segment
  ( Segment (..)
  , domain
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

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.VectorBounds3D (VectorBounds3D)

data Segment space = Segment
  { domain :: UvBounds
  , range :: ~(Bounds3D space)
  , partialDerivativeRanges :: (VectorBounds3D Meters space, VectorBounds3D Meters space)
  , secondPartialDerivativeRanges ::
      ( VectorBounds3D Meters space
      , VectorBounds3D Meters space
      , VectorBounds3D Meters space
      )
  , normalDirectionRange :: ~(DirectionBounds3D space)
  , isDegenerate :: ~Bool
  , isMonotonic :: ~Bool
  , bottomLeftChild :: ~(Segment space)
  , bottomRightChild :: ~(Segment space)
  , topLeftChild :: ~(Segment space)
  , topRightChild :: ~(Segment space)
  , leftNeighbor :: Maybe (Segment space)
  , rightNeighbor :: Maybe (Segment space)
  , bottomNeighbor :: Maybe (Segment space)
  , topNeighbor :: Maybe (Segment space)
  }

instance Bounded (Segment space) UvBounds where
  bounds = domain

instance space1 ~ space2 => Bounded (Segment space1) (Bounds3D space2) where
  bounds = range

instance Bisection.Segment (Segment space) UvBounds where
  children segment =
    NonEmpty.four
      segment.bottomLeftChild
      segment.bottomRightChild
      segment.topLeftChild
      segment.topRightChild

domain :: Segment space -> UvBounds
domain segment = segment.domain

range :: Segment space -> Bounds3D space
range segment = segment.range

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
