module OpenSolid.Curve.Segment
  ( Segment
  , curve
  , tangentCurve
  , parameterBounds
  , bounds
  , firstDerivativeBounds
  , secondDerivativeBounds
  , tangentBounds
  , evaluate
  , monotonic
  )
where

import OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.DirectionBounds (DirectionBounds)
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

data Segment dimension units space = Segment
  { curve :: Curve dimension units space
  , tangentCurve :: DirectionCurve dimension space
  , parameterBounds :: Interval Unitless
  , bounds :: ~(Bounds dimension units space)
  , firstDerivativeBounds :: ~(VectorBounds dimension units space)
  , secondDerivativeBounds :: ~(VectorBounds dimension units space)
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

tangentBounds :: Segment dimension units space -> DirectionBounds dimension space
tangentBounds = (.tangentBounds)

monotonic :: VectorBounds.Exists dimension units space => Segment dimension units space -> Bool
monotonic segment = not (VectorBounds.includes Vector.zero segment.firstDerivativeBounds)

evaluate ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  ) =>
  Curve dimension units space ->
  DirectionCurve dimension space ->
  Interval Unitless ->
  Segment dimension units space
evaluate givenCurve givenTangentCurve givenParameterBounds = do
  let firstDerivativeCurve = Curve.derivative givenCurve
  let secondDerivativeCurve = VectorCurve.derivative firstDerivativeCurve
  Segment
    { curve = givenCurve
    , tangentCurve = givenTangentCurve
    , parameterBounds = givenParameterBounds
    , bounds = Curve.evaluateBounds givenCurve givenParameterBounds
    , firstDerivativeBounds = VectorCurve.evaluateBounds firstDerivativeCurve givenParameterBounds
    , secondDerivativeBounds = VectorCurve.evaluateBounds secondDerivativeCurve givenParameterBounds
    , tangentBounds = DirectionCurve.evaluateBounds givenTangentCurve givenParameterBounds
    }
