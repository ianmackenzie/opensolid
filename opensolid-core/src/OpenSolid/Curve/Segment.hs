module OpenSolid.Curve.Segment
  ( Segment
  , curve
  , tangentCurve
  , parameterBounds
  , bounds
  , firstDerivativeBounds
  , secondDerivativeBounds
  , curvatureVectorBounds_
  , tangentBounds
  , evaluate
  , monotonic
  , crossingTangents
  , distinctCurvatures
  )
where

import OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

data Segment dimension units space = Segment
  { curve :: Curve dimension units space
  , tangentCurve :: DirectionCurve dimension space
  , parameterBounds :: Interval Unitless
  , bounds :: ~(Bounds dimension units space)
  , firstDerivativeBounds :: ~(VectorBounds dimension units space)
  , secondDerivativeBounds :: ~(VectorBounds dimension units space)
  , curvatureVectorBounds_ :: ~(VectorBounds dimension (Unitless ?/? units) space)
  , tangentBounds :: ~(DirectionBounds dimension space)
  }

curve :: Segment dimension units space -> Curve dimension units space
curve = (.curve)

tangentCurve :: Segment dimension units space -> DirectionCurve dimension space
tangentCurve = (.tangentCurve)

parameterBounds :: Segment dimension units space -> Interval Unitless
parameterBounds = (.parameterBounds)

bounds :: Segment dimension units space -> Bounds dimension units space
bounds = (.bounds)

firstDerivativeBounds :: Segment dimension units space -> VectorBounds dimension units space
firstDerivativeBounds = (.firstDerivativeBounds)

secondDerivativeBounds :: Segment dimension units space -> VectorBounds dimension units space
secondDerivativeBounds = (.secondDerivativeBounds)

curvatureVectorBounds_ ::
  Segment dimension units space ->
  VectorBounds dimension (Unitless ?/? units) space
curvatureVectorBounds_ = (.curvatureVectorBounds_)

tangentBounds :: Segment dimension units space -> DirectionBounds dimension space
tangentBounds = (.tangentBounds)

monotonic :: VectorBounds.Exists dimension units space => Segment dimension units space -> Bool
monotonic segment = Interval.isResolved (VectorBounds.magnitude segment.firstDerivativeBounds)

crossingTangents ::
  DirectionBounds.Exists dimension space =>
  Segment dimension units space ->
  Segment dimension units space ->
  Bool
crossingTangents segment1 segment2 = do
  let bounds1 = DirectionBounds.unwrap segment1.tangentBounds
  let bounds2 = DirectionBounds.unwrap segment2.tangentBounds
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

evaluate ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , DirectionCurve.Exists dimension space
  ) =>
  Curve dimension units space ->
  DirectionCurve dimension space ->
  VectorCurve dimension (Unitless ?/? units) space ->
  Interval Unitless ->
  Segment dimension units space
evaluate givenCurve givenTangentCurve givenCurvatureVectorCurve_ givenParameterBounds = do
  let firstDerivativeCurve = Curve.derivative givenCurve
  let secondDerivativeCurve = VectorCurve.derivative firstDerivativeCurve
  Segment
    { curve = givenCurve
    , tangentCurve = givenTangentCurve
    , parameterBounds = givenParameterBounds
    , bounds = Curve.evaluateBounds givenCurve givenParameterBounds
    , firstDerivativeBounds = VectorCurve.evaluateBounds firstDerivativeCurve givenParameterBounds
    , secondDerivativeBounds = VectorCurve.evaluateBounds secondDerivativeCurve givenParameterBounds
    , curvatureVectorBounds_ = VectorCurve.evaluateBounds givenCurvatureVectorCurve_ givenParameterBounds
    , tangentBounds = DirectionCurve.evaluateBounds givenTangentCurve givenParameterBounds
    }
