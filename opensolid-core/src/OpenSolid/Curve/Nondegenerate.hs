module OpenSolid.Curve.Nondegenerate
  ( tangentDirection
  , tangentDirectionValue
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate

tangentDirection ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  DirectionCurve dimension space
tangentDirection (Nondegenerate curve) =
  VectorCurve.Nondegenerate.direction (Nondegenerate (Curve.derivative curve))

tangentDirectionValue ::
  ( Curve.Exists dimension units space
  , Direction.Exists dimension space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  Direction dimension space
tangentDirectionValue (Nondegenerate curve) tValue =
  VectorCurve.Nondegenerate.directionValue (Nondegenerate (Curve.derivative curve)) tValue
