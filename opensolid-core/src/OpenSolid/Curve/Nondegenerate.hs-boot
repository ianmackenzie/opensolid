module OpenSolid.Curve.Nondegenerate
  ( derivative
  , tangentDirectionValue
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.VectorCurve (VectorCurve)

derivative ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
tangentDirectionValue ::
  ( Curve.Exists dimension units space
  , Direction.Exists dimension space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Direction dimension space
