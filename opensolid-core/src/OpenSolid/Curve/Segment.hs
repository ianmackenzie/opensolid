module OpenSolid.Curve.Segment
  ( Segment
  , bounds
  , derivativeBounds
  , secondDerivativeBounds
  , curvatureVectorBounds_
  , tangentDirectionBounds
  , new
  , monotonic
  , crossingTangents
  , distinctCurvatures
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve.Direction qualified as VectorCurve.Direction

data Segment dimension units space = Segment
  { bounds :: ~(Bounds dimension units space)
  , derivativeBounds :: ~(VectorBounds dimension units space)
  , secondDerivativeBounds :: ~(VectorBounds dimension units space)
  , tangentDirectionBounds :: ~(DirectionBounds dimension space)
  , curvatureVectorBounds_ :: ~(VectorBounds dimension (Unitless ?/? units) space)
  }

instance HasUnits (Segment dimension units space) units

instance
  ( dimension1 ~ dimension2
  , space1 ~ space2
  , VectorBounds.Exists dimension1 units1 space1
  , VectorBounds.Exists dimension2 units2 space2
  , VectorBounds.Exists dimension1 (Unitless ?/? units1) space1
  , VectorBounds.Exists dimension2 (Unitless ?/? units2) space2
  , Units.Coercion (Point dimension1 units1 space1) (Point dimension2 units2 space2)
  , Units.Coercion (Bounds dimension1 units1 space1) (Bounds dimension2 units2 space2)
  ) =>
  Units.Coercion
    (Segment dimension1 units1 space1)
    (Segment dimension2 units2 space2)
  where
  coerce segment =
    Segment
      { bounds = Units.coerce segment.bounds
      , derivativeBounds = VectorBounds.coerce segment.derivativeBounds
      , secondDerivativeBounds = VectorBounds.coerce segment.secondDerivativeBounds
      , tangentDirectionBounds = segment.tangentDirectionBounds
      , curvatureVectorBounds_ = VectorBounds.coerce segment.curvatureVectorBounds_
      }

bounds :: Segment dimension units space -> Bounds dimension units space
bounds = (.bounds)

derivativeBounds :: Segment dimension units space -> VectorBounds dimension units space
derivativeBounds = (.derivativeBounds)

secondDerivativeBounds :: Segment dimension units space -> VectorBounds dimension units space
secondDerivativeBounds = (.secondDerivativeBounds)

curvatureVectorBounds_ ::
  Segment dimension units space ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorBounds_ segment = segment.curvatureVectorBounds_

tangentDirectionBounds :: Segment dimension units space -> DirectionBounds dimension space
tangentDirectionBounds segment = segment.tangentDirectionBounds

monotonic :: VectorBounds.Exists dimension units space => Segment dimension units space -> Bool
monotonic segment = Interval.isResolved (VectorBounds.magnitude segment.derivativeBounds)

crossingTangents ::
  DirectionBounds.Exists dimension space =>
  Nonzero (Segment dimension units space) ->
  Nonzero (Segment dimension units space) ->
  Bool
crossingTangents (Nonzero segment1) (Nonzero segment2) = do
  let bounds1 = DirectionBounds.unwrap segment1.tangentDirectionBounds
  let bounds2 = DirectionBounds.unwrap segment2.tangentDirectionBounds
  let notEqual = Interval.isResolved (VectorBounds.magnitude (bounds1 - bounds2))
  let notOpposite = Interval.isResolved (VectorBounds.magnitude (bounds1 + bounds2))
  notEqual && notOpposite

distinctCurvatures ::
  ( Curve.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  ) =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
distinctCurvatures segment1 segment2 = do
  let bounds1 = curvatureVectorBounds_ segment1
  let bounds2 = curvatureVectorBounds_ segment2
  Interval.isResolved (VectorBounds.magnitude (bounds1 - bounds2))

new ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , Bounds.Exists dimension units space
  , DirectionBounds.Exists dimension space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  , Addition
      (Point dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  , Subtraction
      (Point dimension units space)
      (VectorBounds dimension units space)
      (Bounds dimension units space)
  ) =>
  Curve dimension units space ->
  Interval Unitless ->
  Segment dimension units space
new givenCurve tBounds = do
  let Interval t1 t2 = tBounds
  let p1 = Curve.point givenCurve t1
  let p2 = Curve.point givenCurve t2
  let segmentBounds0 = Curve.bounds givenCurve tBounds
  let segmentDerivativeBounds = Curve.derivativeBounds givenCurve tBounds
  let segmentSecondDerivativeBounds = Curve.secondDerivativeBounds givenCurve tBounds
  let leftBounds = Bounds.aggregate2 (Bounds.constant p1) (p1 + 0.5 * segmentDerivativeBounds)
  let rightBounds = Bounds.aggregate2 (Bounds.constant p2) (p2 - 0.5 * segmentDerivativeBounds)
  let segmentBounds1 = Bounds.aggregate2 leftBounds rightBounds
  let segmentBounds =
        case Bounds.intersection segmentBounds0 segmentBounds1 of
          Just intersection -> intersection
          -- Shouldn't happen, bounds and derivative bounds should be consistent
          Nothing -> segmentBounds0
  let segmentTangentDirectionBounds =
        VectorCurve.Direction.bounds
          (Curve.derivative givenCurve)
          tBounds
          segmentDerivativeBounds
          segmentSecondDerivativeBounds
  let segmentCurvatureVectorBounds_ =
        Curve.CurvatureVector.bounds_
          segmentDerivativeBounds
          segmentSecondDerivativeBounds
  Segment
    { bounds = segmentBounds
    , derivativeBounds = segmentDerivativeBounds
    , secondDerivativeBounds = segmentSecondDerivativeBounds
    , tangentDirectionBounds = segmentTangentDirectionBounds
    , curvatureVectorBounds_ = segmentCurvatureVectorBounds_
    }
