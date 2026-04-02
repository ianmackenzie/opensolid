{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson2D (EvaluateCurve, curve, EvaluateSurface, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D

type EvaluateCurve units = Number -> (# Vector2D units, Vector2D units #)

curve :: EvaluateCurve units -> Number -> Number
curve evaluate t1 = do
  let (# v1, d1 #) = evaluate t1
  curveImpl evaluate t1 v1 d1

curveImpl :: EvaluateCurve units -> Number -> Vector2D units -> Vector2D units -> Number
curveImpl evaluate t1 v1 d1 = do
  let t2 = t1 - (v1 `dot_` d1) / (d1 `dot_` d1)
  let (# v2, d2 #) = evaluate t2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then curveImpl evaluate t2 v2 d2
    else t1

type EvaluateSurface units = UvPoint -> (# Vector2D units, Vector2D units, Vector2D units #)

surface :: EvaluateSurface units -> UvPoint -> UvPoint
surface evaluate uvPoint1 = do
  let (# v1, du1, dv1 #) = evaluate uvPoint1
  surfaceImpl evaluate uvPoint1 v1 du1 dv1

surfaceImpl ::
  EvaluateSurface units ->
  UvPoint ->
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  UvPoint
surfaceImpl evaluate uvPoint1 v1 du1 dv1 = do
  let Vector2D x1 y1 = v1
  let Vector2D dxdu1 dydu1 = du1
  let Vector2D dxdv1 dydv1 = dv1
  let determinant = dxdu1 ?*? dydv1 - dxdv1 ?*? dydu1
  let uStep = (dxdv1 ?*? y1 - dydv1 ?*? x1) / determinant
  let vStep = (dydu1 ?*? x1 - dxdu1 ?*? y1) / determinant
  let uvPoint2 = uvPoint1 + Vector2D uStep vStep
  let (# v2, du2, dv2 #) = evaluate uvPoint2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then surfaceImpl evaluate uvPoint2 v2 du2 dv2
    else uvPoint1
