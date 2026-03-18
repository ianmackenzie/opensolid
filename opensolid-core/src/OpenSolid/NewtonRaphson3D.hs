{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson3D (EvaluateCurve, curve, EvaluateSurface, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

type EvaluateCurve units space = Number -> (# Vector3D units space, Vector3D units space #)

curve :: EvaluateCurve units space -> Number -> Number
curve evaluate t1 = do
  let (# v1, d1 #) = evaluate t1
  curveImpl evaluate t1 v1 d1

curveImpl ::
  EvaluateCurve units space ->
  Number ->
  Vector3D units space ->
  Vector3D units space ->
  Number
curveImpl evaluate t1 v1 d1 = do
  let t2 = t1 - (v1 `dot_` d1) / (d1 `dot_` d1)
  let (# v2, d2 #) = evaluate t2
  if Vector3D.squaredMagnitude_ v2 < 0.25 * Vector3D.squaredMagnitude_ v1
    then curveImpl evaluate t2 v2 d2
    else t1

type EvaluateSurface units space =
  UvPoint -> (# Vector3D units space, Vector3D units space, Vector3D units space #)

surface ::
  EvaluateSurface units space ->
  UvPoint ->
  UvPoint
surface evaluate uvPoint1 = do
  let (# v1, du1, dv1 #) = evaluate uvPoint1
  surfaceImpl evaluate uvPoint1 v1 du1 dv1

surfaceImpl ::
  EvaluateSurface units space ->
  UvPoint ->
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  UvPoint
surfaceImpl evaluate uvPoint1 v1 du1 dv1 = do
  let uu = du1 `dot_` du1
  let uv = du1 `dot_` dv1
  let vv = dv1 `dot_` dv1
  let determinant = uu ?*? vv - uv ?*? uv
  let x = v1 `dot_` du1
  let y = v1 `dot_` dv1
  let uStep = (uv ?*? y - vv ?*? x) / determinant
  let vStep = (uv ?*? x - uu ?*? y) / determinant
  let uvPoint2 = uvPoint1 + Vector2D uStep vStep
  let (# v2, du2, dv2 #) = evaluate uvPoint2
  if Vector3D.squaredMagnitude_ v2 < 0.25 * Vector3D.squaredMagnitude_ v1
    then surfaceImpl evaluate uvPoint2 v2 du2 dv2
    else uvPoint1
