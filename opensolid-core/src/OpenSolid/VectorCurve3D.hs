module OpenSolid.VectorCurve3D
  ( VectorCurve3D
  , Compiled
  , new
  , compiled
  , isZero
  , hasDegenerateStart
  , hasDegenerateEnd
  , derivative
  , startValue
  , endValue
  , range
  , startDerivative
  , endDerivative
  , derivativeRange
  , startSecondDerivative
  , endSecondDerivative
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
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.IsZero (IsZero)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorCurve (VectorCurve3D)
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorTransform3D (VectorTransform3D)

type Compiled units space = VectorCurve.Compiled 3 units space

{-# INLINE compiled #-}
compiled :: VectorCurve3D units space -> Compiled units space
compiled = VectorCurve.compiled

{-# INLINE derivative #-}
derivative :: VectorCurve3D units space -> VectorCurve3D units space
derivative = VectorCurve.derivative

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

{-# INLINE range #-}
range :: Interval Unitless -> VectorCurve3D units space -> VectorBounds3D units space
range = VectorCurve.range

startDerivative :: VectorCurve3D space units -> Vector3D space units
startDerivative = VectorCurve.startDerivative

endDerivative :: VectorCurve3D space units -> Vector3D space units
endDerivative = VectorCurve.endDerivative

{-# INLINE derivativeRange #-}
derivativeRange :: Interval Unitless -> VectorCurve3D units space -> VectorBounds3D units space
derivativeRange = VectorCurve.derivativeRange

startSecondDerivative :: VectorCurve3D space units -> Vector3D space units
startSecondDerivative = VectorCurve.startSecondDerivative

endSecondDerivative :: VectorCurve3D space units -> Vector3D space units
endSecondDerivative = VectorCurve.endSecondDerivative

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

zeros :: Tolerance units => VectorCurve3D units space -> Result IsZero (List Number)
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
