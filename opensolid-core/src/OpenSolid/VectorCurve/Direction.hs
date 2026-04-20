module OpenSolid.VectorCurve.Direction (range) where

import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Prelude
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

range ::
  ( VectorCurve.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , DirectionBounds.Exists dimension space
  ) =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  DirectionBounds dimension space
range curve (Interval tLow tHigh) curveRange derivativeRange
  | tLow == 0.0 && VectorCurve.singular0 curve = VectorBounds.direction derivativeRange
  | tHigh == 1.0 && VectorCurve.singular1 curve = VectorBounds.direction -derivativeRange
  | otherwise = VectorBounds.direction curveRange
