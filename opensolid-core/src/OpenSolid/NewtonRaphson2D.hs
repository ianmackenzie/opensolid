module OpenSolid.NewtonRaphson2D (curve, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D

curve :: (Number -> Vector2D units space) -> (Number -> Vector2D units space) -> Number -> Number
curve function derivative t1 = curveImpl function derivative t1 (function t1)

curveImpl ::
  (Number -> Vector2D units space) ->
  (Number -> Vector2D units space) ->
  Number ->
  Vector2D units space ->
  Number
curveImpl function derivative t1 v1 = do
  let d1 = derivative t1
  let t2 = t1 - (v1 `dot_` d1) / (d1 `dot_` d1)
  let v2 = function t2
  if Vector2D.squaredMagnitude_ v2 < 0.5 * Vector2D.squaredMagnitude_ v1
    then curveImpl function derivative t2 v2
    else t1

surface ::
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvPoint ->
  UvPoint
surface function uDerivative vDerivative uvPoint =
  surfaceImpl function uDerivative vDerivative uvPoint (function uvPoint)

surfaceImpl ::
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvPoint ->
  Vector2D units space ->
  UvPoint
surfaceImpl function uDerivative vDerivative uvPoint1 value1 = do
  let Vector2D x1 y1 = value1
  let Vector2D dxdu1 dydu1 = uDerivative uvPoint1
  let Vector2D dxdv1 dydv1 = vDerivative uvPoint1
  let determinant = dxdu1 ?*? dydv1 - dxdv1 ?*? dydu1
  let uStep = (dxdv1 ?*? y1 - dydv1 ?*? x1) / determinant
  let vStep = (dydu1 ?*? x1 - dxdu1 ?*? y1) / determinant
  let uvPoint2 = uvPoint1 + Vector2D uStep vStep
  let value2 = function uvPoint2
  if Vector2D.squaredMagnitude_ value2 < 0.5 * Vector2D.squaredMagnitude_ value1
    then surfaceImpl function uDerivative vDerivative uvPoint2 value2
    else uvPoint1
