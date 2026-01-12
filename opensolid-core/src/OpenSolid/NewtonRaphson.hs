{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson
  ( curve1D
  , curve1D#
  , curve2D
  , curve2D#
  , surface2D
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D

curve1D :: Tolerance units => Curve units -> Number -> Number
curve1D curve x1 = do
  let function = Curve.evaluate curve
  let derivative = Curve.evaluate curve.derivative
  curve1dImpl function derivative x1 (function x1)

curve1dImpl ::
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Number ->
  Quantity units ->
  Number
curve1dImpl function derivative x1 y1 = do
  let dy1 = derivative x1
  let x2 = x1 .-. y1 ./. dy1
  let y2 = function x2
  if Quantity.abs y2 < 0.5 *. Quantity.abs y1
    then curve1dImpl function derivative x2 y2
    else x1

curve1D# ::
  (Double# -> Double#) ->
  (Double# -> Double#) ->
  Number ->
  Number
curve1D# function# derivative# (Quantity# x1#) =
  curve1dImpl# function# derivative# x1# (function# x1#)

curve1dImpl# ::
  (Double# -> Double#) ->
  (Double# -> Double#) ->
  Double# ->
  Double# ->
  Number
curve1dImpl# function# derivative# x1# y1# = do
  let dy1# = derivative# x1#
  let x2# = x1# -# y1# /# dy1#
  let y2# = function# x2#
  case abs# y2# <# 0.5## *# abs# y1# of
    1# -> curve1dImpl# function# derivative# x2# y2#
    _ -> Quantity (D# x1#)

curve2D :: Tolerance units => VectorCurve2D units space -> Number -> Number
curve2D curve t1 = do
  let function = VectorCurve2D.evaluate curve
  let derivative = VectorCurve2D.evaluate (VectorCurve2D.derivative curve)
  curve2dImpl function derivative t1 (function t1)

curve2dImpl ::
  (Number -> Vector2D units space) ->
  (Number -> Vector2D units space) ->
  Number ->
  Vector2D units space ->
  Number
curve2dImpl function derivative t1 v1 = do
  let d1 = derivative t1
  let t2 = t1 .-. (v1 `dot_` d1) ./. (d1 `dot_` d1)
  let v2 = function t2
  if Vector2D.squaredMagnitude_ v2 < 0.5 *. Vector2D.squaredMagnitude_ v1
    then curve2dImpl function derivative t2 v2
    else t1

curve2D# ::
  (Double# -> (# Double#, Double# #)) ->
  (Double# -> (# Double#, Double# #)) ->
  Number ->
  Number
curve2D# function# derivative# (Quantity# t1#) = do
  let !(# x1#, y1# #) = function# t1#
  let squaredMagnitude1# = x1# *# x1# +# y1# *# y1#
  curve2dImpl# function# derivative# t1# x1# y1# squaredMagnitude1#

curve2dImpl# ::
  (Double# -> (# Double#, Double# #)) ->
  (Double# -> (# Double#, Double# #)) ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Number
curve2dImpl# function# derivative# t1# x1# y1# squaredMagnitude1# = do
  let !(# dx1#, dy1# #) = derivative# t1#
  let t2# = t1# -# (x1# *# dx1# +# y1# *# dy1#) /# (dx1# *# dx1# +# dy1# *# dy1#)
  let !(# x2#, y2# #) = function# t2#
  let squaredMagnitude2# = x2# *# x2# +# y2# *# y2#
  case squaredMagnitude2# <# 0.5## *# squaredMagnitude1# of
    1# -> curve2dImpl# function# derivative# t2# x2# y2# squaredMagnitude2#
    _ -> Quantity (D# t1#)

surface2D :: VectorSurfaceFunction2D units space -> UvPoint -> UvPoint
surface2D surface uvPoint1 = do
  let function = VectorSurfaceFunction2D.evaluate surface
  let uDerivative = VectorSurfaceFunction2D.evaluate surface.du
  let vDerivative = VectorSurfaceFunction2D.evaluate surface.dv
  surface2dImpl function uDerivative vDerivative uvPoint1 (function uvPoint1)

surface2dImpl ::
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvPoint ->
  Vector2D units space ->
  UvPoint
surface2dImpl function uDerivative vDerivative uvPoint1 value1 = do
  let Vector2D x1 y1 = value1
  let Vector2D dxdu1 dydu1 = uDerivative uvPoint1
  let Vector2D dxdv1 dydv1 = vDerivative uvPoint1
  let determinant = dxdu1 ?*? dydv1 .-. dxdv1 ?*? dydu1
  let uStep = (dxdv1 ?*? y1 .-. dydv1 ?*? x1) ./. determinant
  let vStep = (dydu1 ?*? x1 .-. dxdu1 ?*? y1) ./. determinant
  let uvPoint2 = uvPoint1 .+. Vector2D uStep vStep
  let value2 = function uvPoint2
  if Vector2D.squaredMagnitude_ value2 < 0.5 *. Vector2D.squaredMagnitude_ value1
    then surface2dImpl function uDerivative vDerivative uvPoint2 value2
    else uvPoint1
