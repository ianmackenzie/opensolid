module OpenSolid.Curve.Segment
  ( Segment
  , bounds
  , derivativeBounds
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
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds

data Segment dimension units space = Segment
  { bounds :: ~(Bounds dimension units space)
  , derivativeBounds :: ~(VectorBounds dimension units space)
  , secondDerivativeBounds :: ~(VectorBounds dimension units space)
  , tangentBounds :: ~(DirectionBounds dimension space)
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
      , tangentBounds = segment.tangentBounds
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

tangentBounds :: Segment dimension units space -> DirectionBounds dimension space
tangentBounds segment = segment.tangentBounds

monotonic :: VectorBounds.Exists dimension units space => Segment dimension units space -> Bool
monotonic segment = Interval.isResolved (VectorBounds.magnitude segment.derivativeBounds)

crossingTangents ::
  DirectionBounds.Exists dimension space =>
  Nonzero (Segment dimension units space) ->
  Nonzero (Segment dimension units space) ->
  Bool
crossingTangents (Nonzero segment1) (Nonzero segment2) = do
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
  , DirectionBounds.Exists dimension space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  ) =>
  Curve dimension units space ->
  Interval Unitless ->
  Segment dimension units space
evaluate givenCurve givenParameterBounds = do
  let curveBounds = Curve.bounds givenCurve givenParameterBounds
  let curveDerivativeBounds = Curve.derivativeBounds givenCurve givenParameterBounds
  let curveSecondDerivativeBounds = Curve.secondDerivativeBounds givenCurve givenParameterBounds
  Segment
    { bounds = curveBounds
    , derivativeBounds = curveDerivativeBounds
    , secondDerivativeBounds = curveSecondDerivativeBounds
    , tangentBounds =
        DirectionBounds.unsafe
          (curveDerivativeBounds / VectorBounds.magnitude curveDerivativeBounds)
    , curvatureVectorBounds_ =
        Curve.CurvatureVector.bounds_ curveDerivativeBounds curveSecondDerivativeBounds
    }
