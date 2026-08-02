module OpenSolid.VectorCurve.Direction (value, range) where

import OpenSolid.Direction (Direction, DirectionExists)
import OpenSolid.DirectionBounds (DirectionBounds, DirectionBoundsExists)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Vector (Vector, VectorExists)
import OpenSolid.Vector.Nonzero qualified as Vector.Nonzero
import OpenSolid.VectorBounds (VectorBounds, VectorBoundsExists)
import OpenSolid.VectorBounds qualified as VectorBounds
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve, VectorCurveExists)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

value ::
  ( VectorCurveExists dimension units space
  , VectorExists dimension units space
  , DirectionExists dimension space
  ) =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space ->
  Vector dimension units space ->
  Direction dimension space
value curve tValue curveValue derivativeValue =
  Vector.Nonzero.direction . Nonzero $
    if
      | tValue == 0.0 && VectorCurve.hasDegenerateStart curve -> derivativeValue
      | tValue == 1.0 && VectorCurve.hasDegenerateEnd curve -> -derivativeValue
      | otherwise -> curveValue

range ::
  ( VectorCurveExists dimension units space
  , VectorBoundsExists dimension units space
  , DirectionBoundsExists dimension space
  ) =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  DirectionBounds dimension space
range curve (Interval tLow tHigh) curveRange derivativeRange =
  VectorBounds.direction $
    if
      | tLow == 0.0 && VectorCurve.hasDegenerateStart curve -> derivativeRange
      | tHigh == 1.0 && VectorCurve.hasDegenerateEnd curve -> -derivativeRange
      | otherwise -> curveRange
