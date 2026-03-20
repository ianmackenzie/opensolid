module OpenSolid.Curve.Segment
  ( Segment
  , startPoint
  , endPoint
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
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.CurvatureVector qualified as Curve.CurvatureVector
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds

data Segment dimension units space = Segment
  { startPoint :: ~(Point dimension units space)
  , endPoint :: ~(Point dimension units space)
  , bounds :: ~(Bounds dimension units space)
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
      { startPoint = Units.coerce segment.startPoint
      , endPoint = Units.coerce segment.endPoint
      , bounds = Units.coerce segment.bounds
      , derivativeBounds = VectorBounds.coerce segment.derivativeBounds
      , secondDerivativeBounds = VectorBounds.coerce segment.secondDerivativeBounds
      , tangentDirectionBounds = segment.tangentDirectionBounds
      , curvatureVectorBounds_ = VectorBounds.coerce segment.curvatureVectorBounds_
      }

startPoint :: Segment dimension units space -> Point dimension units space
startPoint = (.startPoint)

endPoint :: Segment dimension units space -> Point dimension units space
endPoint = (.endPoint)

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
  , DirectionBounds.Exists dimension space
  , VectorBounds.Exists dimension units space
  , VectorBounds.Exists dimension (Unitless ?/? units) space
  ) =>
  Curve dimension units space ->
  Interval Unitless ->
  Segment dimension units space
new givenCurve givenParameterBounds = do
  let curveStartPoint = Curve.startPoint givenCurve
  let curveEndPoint = Curve.endPoint givenCurve
  let curveBounds = Curve.bounds givenCurve givenParameterBounds
  let curveDerivativeBounds = Curve.derivativeBounds givenCurve givenParameterBounds
  let curveSecondDerivativeBounds = Curve.secondDerivativeBounds givenCurve givenParameterBounds
  Segment
    { startPoint = curveStartPoint
    , endPoint = curveEndPoint
    , bounds = curveBounds
    , derivativeBounds = curveDerivativeBounds
    , secondDerivativeBounds = curveSecondDerivativeBounds
    , tangentDirectionBounds =
        DirectionBounds.unsafe
          (curveDerivativeBounds / VectorBounds.magnitude curveDerivativeBounds)
    , curvatureVectorBounds_ =
        Curve.CurvatureVector.bounds_ curveDerivativeBounds curveSecondDerivativeBounds
    }
