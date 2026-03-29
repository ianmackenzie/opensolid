module OpenSolid.VectorCurve.Nondegenerate
  ( magnitude
  , direction
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Nondegenerate (Nondegenerate)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

magnitude ::
  VectorCurve.Exists dimension units space =>
  Nondegenerate (VectorCurve dimension units space) ->
  Nondegenerate (Curve1D units)
direction ::
  VectorCurve.Exists dimension units space =>
  Nondegenerate (VectorCurve dimension units space) ->
  DirectionCurve dimension space
