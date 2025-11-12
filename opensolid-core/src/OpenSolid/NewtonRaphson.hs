{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson
  ( Divergence (Divergence)
  , curve1d
  , curve1d##
  , curve2d
  , curve2d##
  , surface2d
  )
where

import GHC.Exts qualified
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

data Divergence = Divergence deriving (Eq, Show)

curve1d :: Tolerance units => Curve units -> Number -> Result Divergence Number
curve1d curve x1 = do
  let function = Curve.evaluate curve
  let derivative = Curve.evaluate curve.derivative
  curve1dImpl function derivative x1 (function x1) 0

curve1dImpl ::
  Tolerance units =>
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Number ->
  Quantity units ->
  Int ->
  Result Divergence Number
curve1dImpl function derivative x1 y1 iterations =
  if iterations <= 10
    then do
      let dy1 = derivative x1
      let x2 = x1 .-. y1 ./. dy1
      let y2 = function x2
      if Quantity.abs y2 < Quantity.abs y1
        then curve1dImpl function derivative x2 y2 (iterations + 1)
        else if y1 ~= Quantity.zero then Ok x1 else Error Divergence
    else Error Divergence

curve1d## ::
  Tolerance units =>
  (Double# -> Double#) ->
  (Double# -> Double#) ->
  Number ->
  Result Divergence Number
curve1d## function## derivative## (Quantity## x1##) = do
  let !(Quantity## tolerance##) = ?tolerance
  curve1dImpl## tolerance## function## derivative## x1## (function## x1##) 0#

curve1dImpl## ::
  Double# ->
  (Double# -> Double#) ->
  (Double# -> Double#) ->
  Double# ->
  Double# ->
  Int# ->
  Result Divergence Number
curve1dImpl## tolerance## function## derivative## x1## y1## iterations1# =
  case iterations1# GHC.Exts.<=# 10# of
    1# -> do
      let dy1## = derivative## x1##
      let x2## = x1## -## y1## /## dy1##
      let y2## = function## x2##
      case abs## y2## <## abs## y1## of
        1# -> do
          let iterations2# = iterations1# GHC.Exts.+# 1#
          curve1dImpl## tolerance## function## derivative## x2## y2## iterations2#
        _ -> case abs## y1## <=## tolerance## of
          1# -> Ok (Quantity (D# x1##))
          _ -> Error Divergence
    _ -> Error Divergence

curve2d :: Tolerance units => VectorCurve2d (space @ units) -> Number -> Result Divergence Number
curve2d curve t1 = do
  let function = VectorCurve2d.evaluate curve
  let derivative = VectorCurve2d.evaluate curve.derivative
  curve2dImpl function derivative t1 (function t1) 0

curve2dImpl ::
  Tolerance units =>
  (Number -> Vector2d (space @ units)) ->
  (Number -> Vector2d (space @ units)) ->
  Number ->
  Vector2d (space @ units) ->
  Int ->
  Result Divergence Number
curve2dImpl function derivative t1 v1 iterations =
  if iterations <= 10
    then do
      let d1 = derivative t1
      let t2 = t1 .-. (v1 `dot#` d1) ./. (d1 `dot#` d1)
      let v2 = function t2
      if Vector2d.squaredMagnitude# v2 < Vector2d.squaredMagnitude# v1
        then curve2dImpl function derivative t2 v2 (iterations + 1)
        else if v1 ~= Vector2d.zero then Ok t1 else Error Divergence
    else Error Divergence

curve2d## ::
  Tolerance units =>
  (Double# -> (# Double#, Double# #)) ->
  (Double# -> (# Double#, Double# #)) ->
  Number ->
  Result Divergence Number
curve2d## function## derivative## (Quantity## t1##) = do
  let !(Quantity## tolerance##) = ?tolerance
  let !(# x1##, y1## #) = function## t1##
  let squaredMagnitude1## = x1## *## x1## +## y1## *## y1##
  curve2dImpl## tolerance## function## derivative## t1## x1## y1## squaredMagnitude1## 0#

curve2dImpl## ::
  Double# ->
  (Double# -> (# Double#, Double# #)) ->
  (Double# -> (# Double#, Double# #)) ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Int# ->
  Result Divergence Number
curve2dImpl## tolerance## function## derivative## t1## x1## y1## squaredMagnitude1## iterations1# =
  case iterations1# GHC.Exts.<=# 10# of
    1# -> do
      let !(# dx1##, dy1## #) = derivative## t1##
      let t2## =
            t1##
              -## (x1## *## dx1## +## y1## *## dy1##)
              /## (dx1## *## dx1## +## dy1## *## dy1##)
      let !(# x2##, y2## #) = function## t2##
      let squaredMagnitude2## = x2## *## x2## +## y2## *## y2##
      case squaredMagnitude2## <## squaredMagnitude1## of
        1# -> do
          let iterations2# = iterations1# GHC.Exts.+# 1#
          curve2dImpl## tolerance## function## derivative## t2## x2## y2## squaredMagnitude2## iterations2#
        _ -> case squaredMagnitude2## <=## tolerance## *## tolerance## of
          1# -> Ok (Quantity (D# t1##))
          _ -> Error Divergence
    _ -> Error Divergence

surface2d ::
  Tolerance units =>
  VectorSurfaceFunction2d (space @ units) ->
  UvPoint ->
  Result Divergence UvPoint
surface2d surface uvPoint1 = do
  let function = VectorSurfaceFunction2d.evaluate surface
  let uDerivative = VectorSurfaceFunction2d.evaluate surface.du
  let vDerivative = VectorSurfaceFunction2d.evaluate surface.dv
  surface2dImpl function uDerivative vDerivative uvPoint1 (function uvPoint1) 0

surface2dImpl ::
  Tolerance units =>
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  UvPoint ->
  Vector2d (space @ units) ->
  Int ->
  Result Divergence UvPoint
surface2dImpl function uDerivative vDerivative uvPoint1 value1 iterations =
  if iterations <= 10
    then do
      let Vector2d x1 y1 = value1
      let Vector2d dxdu1 dydu1 = uDerivative uvPoint1
      let Vector2d dxdv1 dydv1 = vDerivative uvPoint1
      let determinant = dxdu1 #*# dydv1 .-. dxdv1 #*# dydu1
      let uStep = (dxdv1 #*# y1 .-. dydv1 #*# x1) ./. determinant
      let vStep = (dydu1 #*# x1 .-. dxdu1 #*# y1) ./. determinant
      let uvPoint2 = uvPoint1 .+. Vector2d uStep vStep
      let value2 = function uvPoint2
      if Vector2d.squaredMagnitude# value2 < Vector2d.squaredMagnitude# value1
        then surface2dImpl function uDerivative vDerivative uvPoint2 value2 (iterations + 1)
        else if value1 ~= Vector2d.zero then Ok uvPoint1 else Error Divergence
    else Error Divergence
