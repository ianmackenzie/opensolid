module OpenSolid.NewtonRaphson3D (curve) where

import OpenSolid.Prelude
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
  if Vector3D.squaredMagnitude_ v2 < 0.5 * Vector3D.squaredMagnitude_ v1
    then curveImpl function derivative t2 v2
    else t1
