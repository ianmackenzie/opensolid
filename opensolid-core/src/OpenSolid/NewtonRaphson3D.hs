module OpenSolid.NewtonRaphson3D (curve, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

curve :: (Number -> Vector3D units space) -> (Number -> Vector3D units space) -> Number -> Number
curve function derivative t1 = curveImpl function derivative t1 (function t1)

curveImpl ::
  (Number -> Vector3D units space) ->
  (Number -> Vector3D units space) ->
  Number ->
  Vector3D units space ->
  Number
curveImpl function derivative t1 v1 = do
  let d1 = derivative t1
  let t2 = t1 - (v1 `dot_` d1) / (d1 `dot_` d1)
  let v2 = function t2
  if Vector3D.squaredMagnitude_ v2 < 0.25 * Vector3D.squaredMagnitude_ v1
    then curveImpl function derivative t2 v2
    else t1

surface ::
  (UvPoint -> Vector3D units space) ->
  (UvPoint -> Vector3D units space) ->
  (UvPoint -> Vector3D units space) ->
  UvPoint ->
  UvPoint
surface function uDerivative vDerivative uvPoint =
  surfaceImpl function uDerivative vDerivative uvPoint (function uvPoint)

surfaceImpl ::
  (UvPoint -> Vector3D units space) ->
  (UvPoint -> Vector3D units space) ->
  (UvPoint -> Vector3D units space) ->
  UvPoint ->
  Vector3D units space ->
  UvPoint
surfaceImpl function uDerivative vDerivative uvPoint1 value1 = do
  let du = uDerivative uvPoint1
  let dv = vDerivative uvPoint1
  let uu = du `dot_` du
  let uv = du `dot_` dv
  let vv = dv `dot_` dv
  let determinant = uu ?*? vv - uv ?*? uv
  let x = value1 `dot_` du
  let y = value1 `dot_` dv
  let uStep = (uv ?*? y - vv ?*? x) / determinant
  let vStep = (uv ?*? x - uu ?*? y) / determinant
  let uvPoint2 = uvPoint1 + (Vector2D uStep vStep)
  let value2 = function uvPoint2
  if Vector3D.squaredMagnitude_ value2 < 0.25 * Vector3D.squaredMagnitude_ value1
    then surfaceImpl function uDerivative vDerivative uvPoint2 value2
    else uvPoint1
