module OpenSolid.Curve.Nondegenerate
  ( derivative
  , derivativeAt
  , secondDerivativeAt
  , tangentDirectionAt
  , findPoint
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Direction (Direction, DirectionExists)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorCurve (VectorCurve)

derivative ::
  CurveExists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (VectorCurve dimension units space)
derivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
secondDerivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Vector dimension units space
tangentDirectionAt ::
  (CurveExists dimension units space, DirectionExists dimension space) =>
  Number ->
  Nondegenerate (Curve dimension units space) ->
  Direction dimension space
findPoint ::
  (CurveExists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Nondegenerate (Curve dimension units space) ->
  List Number
