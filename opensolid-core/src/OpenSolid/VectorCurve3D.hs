module OpenSolid.VectorCurve3D
  ( VectorCurve3D
  , Compiled
  , Nondegenerate
  , new
  , on
  , compiled
  , isZero
  , singular0
  , singular1
  , derivative
  , nondegenerate
  , startValue
  , endValue
  , value
  , bounds
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , zero
  , constant
  , interpolateFrom
  , arc
  , quadraticBezier
  , cubicBezier
  , bezier
  , desingularize
  , desingularized
  , quotient
  , quotient_
  , magnitude
  , squaredMagnitude
  , squaredMagnitude_
  , reverse
  , zeros
  , direction
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (IsDegenerate, Nondegenerate)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

type VectorCurve3D units space = VectorCurve 3 units space

type Compiled units space = VectorCurve.Compiled 3 units space

compiled :: VectorCurve3D units space -> Compiled units space
compiled = VectorCurve.compiled

derivative :: VectorCurve3D units space -> VectorCurve3D units space
derivative = VectorCurve.derivative

nondegenerate ::
  Tolerance units =>
  VectorCurve3D units space ->
  Result IsDegenerate (Nondegenerate (VectorCurve3D units space))
nondegenerate = VectorCurve.nondegenerate

isZero :: Tolerance units => VectorCurve3D units space -> Bool
isZero = VectorCurve.isZero

singular0 :: VectorCurve3D units space -> Bool
singular0 = VectorCurve.singular0

singular1 :: VectorCurve3D units space -> Bool
singular1 = VectorCurve.singular1

transformBy :: Transform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector3D.transformBy transform)
          (VectorBounds3D.transformBy transform)
          (compiled curve)
  new compiledTransformed (transformBy transform (derivative curve))

new :: Compiled units space -> VectorCurve3D units space -> VectorCurve3D units space
new = VectorCurve.new

zero :: VectorCurve3D units space
zero = VectorCurve.zero

constant :: Vector3D units space -> VectorCurve3D units space
constant = VectorCurve.constant

on :: Plane3D space -> VectorCurve2D units -> VectorCurve3D units space
on plane vectorCurve2D = do
  let compiledPlanar =
        CompiledFunction.map
          (Expression.placeOn plane)
          (Vector2D.placeOn plane)
          (VectorBounds2D.placeOn plane)
          (VectorCurve2D.compiled vectorCurve2D)
  let planarDerivative = on plane (VectorCurve2D.derivative vectorCurve2D)
  new compiledPlanar planarDerivative

interpolateFrom :: Vector3D units space -> Vector3D units space -> VectorCurve3D units space
interpolateFrom = VectorCurve.interpolateFrom

arc ::
  Vector3D units space ->
  Vector3D units space ->
  Angle ->
  Angle ->
  VectorCurve3D units space
arc = VectorCurve.arc

quadraticBezier ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorCurve3D units space
quadraticBezier = VectorCurve.quadraticBezier

cubicBezier ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorCurve3D units space
cubicBezier = VectorCurve.cubicBezier

bezier :: NonEmpty (Vector3D units space) -> VectorCurve3D units space
bezier = VectorCurve.bezier

startValue :: VectorCurve3D units space -> Vector3D units space
startValue = VectorCurve.startValue

endValue :: VectorCurve3D units space -> Vector3D units space
endValue = VectorCurve.endValue

desingularize ::
  Maybe (Vector3D units space, Vector3D units space) ->
  VectorCurve3D units space ->
  Maybe (Vector3D units space, Vector3D units space) ->
  VectorCurve3D units space
desingularize = VectorCurve.desingularize

desingularized ::
  VectorCurve3D units space ->
  VectorCurve3D units space ->
  VectorCurve3D units space ->
  VectorCurve3D units space
desingularized = VectorCurve.desingularized

value :: VectorCurve3D units space -> Number -> Vector3D units space
value = VectorCurve.value

bounds :: VectorCurve3D units space -> Interval Unitless -> VectorBounds3D units space
bounds = VectorCurve.bounds

{-# INLINE derivativeValue #-}
derivativeValue :: VectorCurve3D units space -> Number -> Vector3D units space
derivativeValue = VectorCurve.derivativeValue

{-# INLINE derivativeBounds #-}
derivativeBounds :: VectorCurve3D units space -> Interval Unitless -> VectorBounds3D units space
derivativeBounds = VectorCurve.derivativeBounds

{-# INLINE secondDerivativeValue #-}
secondDerivativeValue :: VectorCurve3D units space -> Number -> Vector3D units space
secondDerivativeValue = VectorCurve.secondDerivativeValue

{-# INLINE secondDerivativeBounds #-}
secondDerivativeBounds ::
  VectorCurve3D units space ->
  Interval Unitless ->
  VectorBounds3D units space
secondDerivativeBounds = VectorCurve.secondDerivativeBounds

reverse :: VectorCurve3D units space -> VectorCurve3D units space
reverse = VectorCurve.reverse

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve3D units3 space)
quotient lhs rhs = Result.map Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve3D (units1 ?/? units2) space)
quotient_ = VectorCurve.quotient_

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3D units1 space -> Curve1D units2
squaredMagnitude = VectorCurve.squaredMagnitude

squaredMagnitude_ :: VectorCurve3D units space -> Curve1D (units ?*? units)
squaredMagnitude_ = VectorCurve.squaredMagnitude_

magnitude :: Tolerance units => VectorCurve3D units space -> Curve1D units
magnitude = VectorCurve.magnitude

zeros :: Tolerance units => VectorCurve3D units space -> Result IsDegenerate (List Number)
zeros = VectorCurve.zeros

direction ::
  Tolerance units =>
  VectorCurve3D units space ->
  Result IsDegenerate (DirectionCurve3D space)
direction = VectorCurve.direction

placeIn ::
  Frame3D global local ->
  VectorCurve3D units local ->
  VectorCurve3D units global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Vector3D.placeIn frame)
          (VectorBounds3D.placeIn frame)
          (compiled curve)
  new compiledPlaced (placeIn frame (derivative curve))

relativeTo ::
  Frame3D global local ->
  VectorCurve3D units global ->
  VectorCurve3D units local
relativeTo frame = placeIn (Frame3D.inverse frame)
