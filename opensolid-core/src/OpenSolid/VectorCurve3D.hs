module OpenSolid.VectorCurve3D
  ( VectorCurve3D
  , Compiled
  , Nondegenerate
  , new
  , on
  , compiled
  , isZero
  , hasDegenerateStart
  , hasDegenerateEnd
  , derivative
  , nondegenerate
  , startValue
  , endValue
  , valueAt
  , valueOf
  , range
  , derivativeAt
  , derivativeRange
  , secondDerivativeAt
  , secondDerivativeRange
  , zero
  , constant
  , interpolateFrom
  , arc
  , quadraticBezier
  , cubicBezier
  , bezier
  , squaredMagnitude
  , squaredMagnitude_
  , reverse
  , zeros
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Error (IsDegenerate)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorCurve (VectorCurve3D)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorTransform3D (VectorTransform3D)

type Compiled units space = VectorCurve.Compiled 3 units space

{-# INLINE compiled #-}
compiled :: VectorCurve3D units space -> Compiled units space
compiled = VectorCurve.compiled

{-# INLINE derivative #-}
derivative :: VectorCurve3D units space -> VectorCurve3D units space
derivative = VectorCurve.derivative

nondegenerate ::
  Tolerance units =>
  VectorCurve3D units space ->
  Result IsDegenerate (Nondegenerate (VectorCurve3D units space))
nondegenerate = VectorCurve.nondegenerate

isZero :: Tolerance units => VectorCurve3D units space -> Bool
isZero = VectorCurve.isZero

hasDegenerateStart :: VectorCurve3D units space -> Bool
hasDegenerateStart = VectorCurve.hasDegenerateStart

hasDegenerateEnd :: VectorCurve3D units space -> Bool
hasDegenerateEnd = VectorCurve.hasDegenerateEnd

transformBy :: VectorTransform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
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

{-# INLINE startValue #-}
startValue :: VectorCurve3D units space -> Vector3D units space
startValue = VectorCurve.startValue

{-# INLINE endValue #-}
endValue :: VectorCurve3D units space -> Vector3D units space
endValue = VectorCurve.endValue

{-# INLINE valueAt #-}
valueAt :: Number -> VectorCurve3D units space -> Vector3D units space
valueAt = VectorCurve.valueAt

{-# INLINE valueOf #-}
valueOf :: VectorCurve3D units space -> Number -> Vector3D units space
valueOf = VectorCurve.valueOf

{-# INLINE range #-}
range :: Interval Unitless -> VectorCurve3D units space -> VectorBounds3D units space
range = VectorCurve.range

{-# INLINE derivativeAt #-}
derivativeAt :: Number -> VectorCurve3D units space -> Vector3D units space
derivativeAt = VectorCurve.derivativeAt

{-# INLINE derivativeRange #-}
derivativeRange :: Interval Unitless -> VectorCurve3D units space -> VectorBounds3D units space
derivativeRange = VectorCurve.derivativeRange

{-# INLINE secondDerivativeAt #-}
secondDerivativeAt :: Number -> VectorCurve3D units space -> Vector3D units space
secondDerivativeAt = VectorCurve.secondDerivativeAt

{-# INLINE secondDerivativeRange #-}
secondDerivativeRange ::
  Interval Unitless ->
  VectorCurve3D units space ->
  VectorBounds3D units space
secondDerivativeRange = VectorCurve.secondDerivativeRange

reverse :: VectorCurve3D units space -> VectorCurve3D units space
reverse = VectorCurve.reverse

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3D units1 space -> Curve1D units2
squaredMagnitude = VectorCurve.squaredMagnitude

squaredMagnitude_ :: VectorCurve3D units space -> Curve1D (units ?*? units)
squaredMagnitude_ = VectorCurve.squaredMagnitude_

zeros :: Tolerance units => VectorCurve3D units space -> Result IsDegenerate (List Number)
zeros = VectorCurve.zeros

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
