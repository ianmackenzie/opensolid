{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson2D (curve, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D

curve :: (Number -> (# Vector2D units space, Vector2D units space #)) -> Number -> Number
curve evaluateFirstOrder t1 = do
  let (# v1, d1 #) = evaluateFirstOrder t1
  curveImpl evaluateFirstOrder t1 v1 d1

curveImpl ::
  (Number -> (# Vector2D units space, Vector2D units space #)) ->
  Number ->
  Vector2D units space ->
  Vector2D units space ->
  Number
curveImpl evaluateFirstOrder t1 v1 d1 = do
  let t2 = t1 - (v1 `dot_` d1) / (d1 `dot_` d1)
  let (# v2, d2 #) = evaluateFirstOrder t2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then curveImpl evaluateFirstOrder t2 v2 d2
    else t1

surface ::
  (UvPoint -> (# Vector2D units space, Vector2D units space, Vector2D units space #)) ->
  UvPoint ->
  UvPoint
surface evaluateFirstOrder uvPoint1 = do
  let (# v1, du1, dv1 #) = evaluateFirstOrder uvPoint1
  surfaceImpl evaluateFirstOrder uvPoint1 v1 du1 dv1

surfaceImpl ::
  (UvPoint -> (# Vector2D units space, Vector2D units space, Vector2D units space #)) ->
  UvPoint ->
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  UvPoint
surfaceImpl evaluateFirstOrder uvPoint1 v1 du1 dv1 = do
  let Vector2D x1 y1 = v1
  let Vector2D dxdu1 dydu1 = du1
  let Vector2D dxdv1 dydv1 = dv1
  let determinant = dxdu1 ?*? dydv1 - dxdv1 ?*? dydu1
  let uStep = (dxdv1 ?*? y1 - dydv1 ?*? x1) / determinant
  let vStep = (dydu1 ?*? x1 - dxdu1 ?*? y1) / determinant
  let uvPoint2 = uvPoint1 + Vector2D uStep vStep
  let (# v2, du2, dv2 #) = evaluateFirstOrder uvPoint2
  if Vector2D.squaredMagnitude_ v2 < 0.25 * Vector2D.squaredMagnitude_ v1
    then surfaceImpl evaluateFirstOrder uvPoint2 v2 du2 dv2
    else uvPoint1
