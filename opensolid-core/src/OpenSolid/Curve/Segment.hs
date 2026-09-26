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

import OpenSolid.Bounds (Bounds, BoundsExists)
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds (VectorBounds, VectorBoundsExists)
import OpenSolid.VectorBounds qualified as VectorBounds

data Segment dimension units space = Segment
  { range :: ~(Bounds dimension units space)
  , derivativeRange :: ~(VectorBounds dimension units space)
  , secondDerivativeRange :: ~(VectorBounds dimension units space)
  , tangentDirectionRange :: ~(DirectionBounds dimension space)
  , curvatureVectorRange_ :: ~(VectorBounds dimension (Unitless ?/? units) space)
  , isDegenerate :: ~Bool
  }

instance Units (Segment dimension units space) units

instance
  ( dimension1 ~ dimension2
  , space1 ~ space2
  , VectorBoundsExists dimension1 units1 space1
  , VectorBoundsExists dimension2 units2 space2
  , VectorBoundsExists dimension1 (Unitless ?/? units1) space1
  , VectorBoundsExists dimension2 (Unitless ?/? units2) space2
  , Units.Coercion (Point dimension1 units1 space1) (Point dimension2 units2 space2)
  , Units.Coercion (Bounds dimension1 units1 space1) (Bounds dimension2 units2 space2)
  ) =>
  Units.Coercion (Segment dimension1 units1 space1) (Segment dimension2 units2 space2)
  where
  coerce segment =
    Segment
      { range = Units.coerce segment.range
      , derivativeRange = VectorBounds.coerce segment.derivativeRange
      , secondDerivativeRange = VectorBounds.coerce segment.secondDerivativeRange
      , tangentDirectionRange = segment.tangentDirectionRange
      , curvatureVectorRange_ = VectorBounds.coerce segment.curvatureVectorRange_
      , isDegenerate = segment.isDegenerate
      }

instance Space.Coercion (Segment 3 Meters space1) (Segment 3 Meters space2) where
  coerce segment =
    Segment
      { range = Space.coerce segment.range
      , derivativeRange = VectorBounds.coerce segment.derivativeRange
      , secondDerivativeRange = VectorBounds.coerce segment.secondDerivativeRange
      , tangentDirectionRange = Space.coerce segment.tangentDirectionRange
      , curvatureVectorRange_ = VectorBounds.coerce segment.curvatureVectorRange_
      , isDegenerate = segment.isDegenerate
      }

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
