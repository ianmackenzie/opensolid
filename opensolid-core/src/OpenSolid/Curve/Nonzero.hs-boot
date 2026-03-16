module OpenSolid.Curve.Nonzero (curvatureVector_) where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Prelude
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

curvatureVector_ ::
  (Curve.Exists dimension units space, VectorCurve.Exists dimension (Unitless ?/? units) space) =>
  Nonzero (Curve dimension units space) ->
  VectorCurve dimension (Unitless ?/? units) space
