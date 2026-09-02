module OpenSolid.Curve.Segment
  ( Segment
  , range
  , derivativeRange
  , secondDerivativeRange
  , curvatureVectorRange_
  , tangentDirectionRange
  , isDegenerate
  , new
  , isMonotonic
  , areDistinct
  , haveCrossingTangents
  , haveDistinctCurvatures
  )
where

import OpenSolid.Bounds (Bounds, BoundsExists)
import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.Curve (Curve, CurveExists)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.Degeneracy qualified as Degeneracy
import OpenSolid.DirectionBounds (DirectionBounds, DirectionBoundsExists)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds (VectorBounds, VectorBoundsExists)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve (VectorCurveExists)
import OpenSolid.VectorCurve.Direction qualified as VectorCurve.Direction

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
  Units.Coercion
    (Segment dimension1 units1 space1)
    (Segment dimension2 units2 space2)
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
isMonotonic segment = Interval.isResolved (VectorBounds.magnitude segment.derivativeRange)

areDistinct ::
  (BoundsExists dimension units space, Tolerance units) =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
areDistinct segment1 segment2 =
  not (range segment1 `intersects` range segment2)

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
  ( CurveExists dimension units space
  , VectorBoundsExists dimension (Unitless ?/? units) space
  ) =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
haveDistinctCurvatures segment1 segment2 =
  VectorBounds.areDistinct
    (curvatureVectorRange_ segment1)
    (curvatureVectorRange_ segment2)

new ::
  ( CurveExists dimension units space
  , VectorCurveExists dimension units space
  , BoundsExists dimension units space
  , DirectionBoundsExists dimension space
  , VectorBoundsExists dimension units space
  , VectorBoundsExists dimension (Unitless ?/? units) space
  , Addition
      (Point dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  , Subtraction
      (Point dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Interval Unitless ->
  Segment dimension units space
new nondegenerateCurve tRange = do
  let Nondegenerate givenCurve = nondegenerateCurve
  let Interval t1 t2 = tRange
  let p1 = Curve.pointAt t1 givenCurve
  let p2 = Curve.pointAt t2 givenCurve
  let segmentRange0 = Curve.range tRange givenCurve
  let segmentDerivativeRange = Curve.derivativeRange tRange givenCurve
  let segmentSecondDerivativeRange = Curve.secondDerivativeRange tRange givenCurve
  let halfDisplacementRange = 0.5 * Interval.width tRange * segmentDerivativeRange
  let leftRange = Bounds.aggregate2 (Bounds.constant p1) (p1 + halfDisplacementRange)
  let rightRange = Bounds.aggregate2 (Bounds.constant p2) (p2 - halfDisplacementRange)
  let segmentRange1 = Bounds.aggregate2 leftRange rightRange
  let segmentRange =
        case Bounds.intersection segmentRange0 segmentRange1 of
          Just intersection -> intersection
          Nothing -> InternalError.throw "Curve bounds and derivative bounds are inconsistent"
  let segmentTangentDirectionRange =
        VectorCurve.Direction.range
          (Curve.derivative givenCurve)
          tRange
          segmentDerivativeRange
          segmentSecondDerivativeRange
  let segmentCurvatureVectorRange_ =
        Curve.CurvatureVector.range_
          segmentDerivativeRange
          segmentSecondDerivativeRange
  let isDegenerateStart = t2 <= Degeneracy.tStart && Curve.hasDegenerateStart givenCurve
  let isDegenerateEnd = t1 >= Degeneracy.tEnd && Curve.hasDegenerateEnd givenCurve
  Segment
    { range = segmentRange
    , derivativeRange = segmentDerivativeRange
    , secondDerivativeRange = segmentSecondDerivativeRange
    , tangentDirectionRange = segmentTangentDirectionRange
    , curvatureVectorRange_ = segmentCurvatureVectorRange_
    , isDegenerate = isDegenerateStart || isDegenerateEnd
    }
