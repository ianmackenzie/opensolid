module OpenSolid.Curve.Nondegenerate
  ( derivative
  , derivativeValue
  , secondDerivativeValue
  , tangentDirection
  , findPoint
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
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
derivativeValue ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Vector dimension units space
secondDerivativeValue ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Vector dimension units space
tangentDirection ::
  (Curve.Exists dimension units space, Direction.Exists dimension space) =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Direction dimension space
findPoint ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List Number
