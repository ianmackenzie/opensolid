module OpenSolid.Curve.Segment
  ( Segment
  , bounds
  , firstDerivativeBounds
  , secondDerivativeBounds
  , tangentBounds
  , evaluate
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
import OpenSolid.VectorBounds (VectorBounds)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

data Segment dimension units space = Segment
  { bounds :: ~(Bounds dimension units space)
  , firstDerivativeBounds :: ~(VectorBounds dimension units space)
  , secondDerivativeBounds :: ~(VectorBounds dimension units space)
  , tangentBounds :: ~(DirectionBounds dimension space)
  }

bounds :: Segment dimension units space -> Bounds dimension units space
bounds = (.bounds)

firstDerivativeBounds :: Segment dimension units space -> VectorBounds dimension units space
firstDerivativeBounds = (.firstDerivativeBounds)

secondDerivativeBounds :: Segment dimension units space -> VectorBounds dimension units space
secondDerivativeBounds = (.secondDerivativeBounds)

tangentBounds :: Segment dimension units space -> DirectionBounds dimension space
tangentBounds = (.tangentBounds)

evaluate ::
  ( Curve.Exists dimension units space
  , VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  ) =>
  Curve dimension units space ->
  DirectionCurve dimension space ->
  Interval Unitless ->
  Segment dimension units space
evaluate curve tangentCurve tBounds = do
  let firstDerivativeCurve = Curve.derivative curve
  let secondDerivativeCurve = VectorCurve.derivative firstDerivativeCurve
  Segment
    { bounds = Curve.evaluateBounds curve tBounds
    , firstDerivativeBounds = VectorCurve.evaluateBounds firstDerivativeCurve tBounds
    , secondDerivativeBounds = VectorCurve.evaluateBounds secondDerivativeCurve tBounds
    , tangentBounds = DirectionCurve.evaluateBounds tangentCurve tBounds
    }
