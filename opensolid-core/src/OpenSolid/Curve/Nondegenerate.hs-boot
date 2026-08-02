module OpenSolid.Curve.Nondegenerate
  ( derivative
  , derivativeAt
  , secondDerivativeAt
  , tangentDirectionAt
  , findPoint
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import OpenSolid.CurvePoint (CurvePoint)
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorCurve (VectorCurve)

derivative ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
derivativeAt ::
  Curve.Exists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
secondDerivativeAt ::
  Curve.Exists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
tangentDirectionAt ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Direction dimension space
findPoint ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List (CurvePoint dimension units space)
