module OpenSolid.Curve.Segment
  ( Segment (..)
  , range
  , derivativeRange
  , secondDerivativeRange
  , curvatureVectorRange_
  , tangentDirectionRange
  , isDegenerate
  , isMonotonic
  , areDistinct
  , haveCrossingTangents
  , haveDistinctCurvatures
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds (Bounds, BoundsExists)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.VectorBounds (VectorBounds, VectorBoundsExists)
import OpenSolid.VectorBounds qualified as VectorBounds

data Segment dimension units space = Segment
  { domain :: Interval Unitless
  , range :: ~(Bounds dimension units space)
  , derivativeRange :: ~(VectorBounds dimension units space)
  , secondDerivativeRange :: ~(VectorBounds dimension units space)
  , tangentDirectionRange :: ~(DirectionBounds dimension space)
  , curvatureVectorRange_ :: ~(VectorBounds dimension (Unitless ?/? units) space)
  , isDegenerate :: ~Bool
  , leftChild :: ~(Segment dimension units space)
  , rightChild :: ~(Segment dimension units space)
  , leftNeighbour :: ~(Maybe (Segment dimension units space))
  , rightNeighbour :: ~(Maybe (Segment dimension units space))
  }

instance Bounded (Segment dimension units space) (Interval Unitless) where
  bounds segment = segment.domain

instance Bounded (Segment 2 units Void) (Bounds2D units) where
  bounds segment = segment.range

instance Bounded (Segment 3 Meters space) (Bounds3D space) where
  bounds segment = segment.range

instance Bisection.Segment (Segment dimension units space) (Interval Unitless) where
  children segment = NonEmpty.two segment.leftChild segment.rightChild

range :: Segment dimension units space -> Bounds dimension units space
range = (.range)

derivativeRange :: Segment dimension units space -> VectorBounds dimension units space
derivativeRange = (.derivativeRange)

secondDerivativeRange :: Segment dimension units space -> VectorBounds dimension units space
secondDerivativeRange = (.secondDerivativeRange)

curvatureVectorRange_ ::
  Segment dimension units space ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorRange_ = (.curvatureVectorRange_)

tangentDirectionRange :: Segment dimension units space -> DirectionBounds dimension space
tangentDirectionRange = (.tangentDirectionRange)

isDegenerate :: Segment dimension units space -> Bool
isDegenerate = (.isDegenerate)

isMonotonic :: VectorBoundsExists dimension units space => Segment dimension units space -> Bool
isMonotonic = VectorBounds.isResolved . derivativeRange

areDistinct ::
  (BoundsExists dimension units space, Tolerance units) =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
areDistinct segment1 segment2 = not (range segment1 ^ range segment2)

haveCrossingTangents ::
  VectorBoundsExists dimension units space =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
haveCrossingTangents segment1 segment2 =
  DirectionBounds.areIndependent
    (tangentDirectionRange segment1)
    (tangentDirectionRange segment2)

haveDistinctCurvatures ::
  VectorBoundsExists dimension (Unitless ?/? units) space =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
haveDistinctCurvatures segment1 segment2 =
  VectorBounds.areDistinct
    (curvatureVectorRange_ segment1)
    (curvatureVectorRange_ segment2)
