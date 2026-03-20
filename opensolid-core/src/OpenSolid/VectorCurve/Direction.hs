module OpenSolid.VectorCurve.Direction (bounds) where

import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Prelude
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

bounds ::
  ( VectorCurve.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , DirectionBounds.Exists dimension space
  ) =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space ->
  VectorBounds dimension units space ->
  DirectionBounds dimension space
bounds curve (Interval tLow tHigh) curveBounds derivativeBounds =
  DirectionBounds.unsafe $
    VectorBounds.normalize $
      if
        | tLow == 0.0 && VectorCurve.singular0 curve -> derivativeBounds
        | tHigh == 1.0 && VectorCurve.singular1 curve -> negate derivativeBounds
        | otherwise -> curveBounds
