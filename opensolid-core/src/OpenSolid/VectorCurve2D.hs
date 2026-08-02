{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorCurve2D
  ( VectorCurve2D
  , Compiled
  , Nondegenerate
  , new
  , compiled
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
  , xComponent
  , yComponent
  , components
  , zero
  , constant
  , xy
  , interpolateFrom
  , arc
  , quadraticBezier
  , cubicBezier
  , bezier
  , squaredMagnitude
  , squaredMagnitude_
  , reverse
  , isZero
  , hasDegenerateStart
  , hasDegenerateEnd
  , zeros
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  , rotateBy
  , convert
  , unconvert
  , newtonRaphson
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Error (IsDegenerate)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve (VectorCurve2D)
import OpenSolid.VectorCurve qualified as VectorCurve
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.VectorTransform2D (VectorTransform2D)
import OpenSolid.VectorTransform2D qualified as VectorTransform2D

type Compiled units = VectorCurve.Compiled 2 units Void

{-# INLINE compiled #-}
compiled :: VectorCurve2D units -> Compiled units
compiled = VectorCurve.compiled

{-# INLINE derivative #-}
derivative :: VectorCurve2D units -> VectorCurve2D units
derivative = VectorCurve.derivative

nondegenerate ::
  Tolerance units =>
  VectorCurve2D units ->
  Result IsDegenerate (Nondegenerate (VectorCurve2D units))
nondegenerate = VectorCurve.nondegenerate

transformBy :: VectorTransform2D tag -> VectorCurve2D units -> VectorCurve2D units
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector2D.transformBy transform)
          (VectorBounds2D.transformBy transform)
          (compiled curve)
  new compiledTransformed (transformBy transform (derivative curve))

rotateBy :: Angle -> VectorCurve2D units -> VectorCurve2D units
rotateBy = VectorTransform2D.rotateByImpl transformBy

new :: Compiled units -> VectorCurve2D units -> VectorCurve2D units
new = VectorCurve.new

-- | The constant zero vector.
zero :: VectorCurve2D units
zero = VectorCurve.zero

-- | Create a curve with a constant value.
constant :: Vector2D units -> VectorCurve2D units
constant = VectorCurve.constant

-- | Create a curve from its X and Y component curves.
xy :: Curve1D units -> Curve1D units -> VectorCurve2D units
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Vector2D
          VectorBounds2D
          (Curve1D.compiled x)
          (Curve1D.compiled y)
  let xyDerivative = xy (Curve1D.derivative x) (Curve1D.derivative y)
  new compiledXY xyDerivative

interpolateFrom :: Vector2D units -> Vector2D units -> VectorCurve2D units
interpolateFrom = VectorCurve.interpolateFrom

arc :: Vector2D units -> Vector2D units -> Angle -> Angle -> VectorCurve2D units
arc = VectorCurve.arc

quadraticBezier ::
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  VectorCurve2D units
quadraticBezier = VectorCurve.quadraticBezier

cubicBezier ::
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  VectorCurve2D units
cubicBezier = VectorCurve.cubicBezier

bezier :: NonEmpty (Vector2D units) -> VectorCurve2D units
bezier = VectorCurve.bezier

{-# INLINE startValue #-}
startValue :: VectorCurve2D units -> Vector2D units
startValue = VectorCurve.startValue

{-# INLINE endValue #-}
endValue :: VectorCurve2D units -> Vector2D units
endValue = VectorCurve.endValue

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
{-# INLINE valueAt #-}
valueAt :: Number -> VectorCurve2D units -> Vector2D units
valueAt = VectorCurve.valueAt

{-# INLINE valueOf #-}
valueOf :: VectorCurve2D units -> Number -> Vector2D units
valueOf = VectorCurve.valueOf

{-# INLINE range #-}
range :: Interval Unitless -> VectorCurve2D units -> VectorBounds2D units
range = VectorCurve.range

{-# INLINE derivativeAt #-}
derivativeAt :: Number -> VectorCurve2D units -> Vector2D units
derivativeAt = VectorCurve.derivativeAt

{-# INLINE derivativeRange #-}
derivativeRange :: Interval Unitless -> VectorCurve2D units -> VectorBounds2D units
derivativeRange = VectorCurve.derivativeRange

{-# INLINE secondDerivativeAt #-}
secondDerivativeAt :: Number -> VectorCurve2D units -> Vector2D units
secondDerivativeAt = VectorCurve.secondDerivativeAt

{-# INLINE secondDerivativeRange #-}
secondDerivativeRange :: Interval Unitless -> VectorCurve2D units -> VectorBounds2D units
secondDerivativeRange = VectorCurve.secondDerivativeRange

-- | Get the X coordinate of a 2D curve as a scalar curve.
xComponent :: VectorCurve2D units -> Curve1D units
xComponent curve = do
  let compiledXComponent =
        CompiledFunction.map
          Expression.xComponent
          Vector2D.xComponent
          VectorBounds2D.xComponent
          (compiled curve)
  Curve1D.new compiledXComponent (xComponent (derivative curve))

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yComponent :: VectorCurve2D units -> Curve1D units
yComponent curve = do
  let compiledYComponent =
        CompiledFunction.map
          Expression.yComponent
          Vector2D.yComponent
          VectorBounds2D.yComponent
          (compiled curve)
  Curve1D.new compiledYComponent (yComponent (derivative curve))

components :: VectorCurve2D units -> (Curve1D units, Curve1D units)
components curve = (xComponent curve, yComponent curve)

reverse :: VectorCurve2D units -> VectorCurve2D units
reverse = VectorCurve.reverse

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2D units1 -> Curve1D units2
squaredMagnitude = VectorCurve.squaredMagnitude

squaredMagnitude_ :: VectorCurve2D units -> Curve1D (units ?*? units)
squaredMagnitude_ = VectorCurve.squaredMagnitude_

isZero :: Tolerance units => VectorCurve2D units -> Bool
isZero = VectorCurve.isZero

hasDegenerateStart :: VectorCurve2D units -> Bool
hasDegenerateStart = VectorCurve.hasDegenerateStart

hasDegenerateEnd :: VectorCurve2D units -> Bool
hasDegenerateEnd = VectorCurve.hasDegenerateEnd

zeros :: Tolerance units => VectorCurve2D units -> Result IsDegenerate (List Number)
zeros = VectorCurve.zeros

placeIn :: Frame2D frameUnits -> VectorCurve2D units -> VectorCurve2D units
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Vector2D.placeIn frame)
          (VectorBounds2D.placeIn frame)
          (compiled curve)
  new compiledPlaced (placeIn frame (derivative curve))

relativeTo :: Frame2D frameUnits -> VectorCurve2D units -> VectorCurve2D units
relativeTo frame = placeIn (Frame2D.inverse frame)

placeOn :: Plane3D space -> VectorCurve2D units -> VectorCurve3D units space
placeOn plane curve = VectorCurve3D.on plane curve

convert :: Quantity (units2 ?/? units1) -> VectorCurve2D units1 -> VectorCurve2D units2
convert factor curve = Units.simplify (curve ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> VectorCurve2D units2 -> VectorCurve2D units1
unconvert factor curve = Units.simplify (curve ?/? factor)

newtonRaphson :: VectorCurve2D units -> Number -> Fuzzy Number
newtonRaphson curve t1 = do
  let evaluate tValue = (# valueAt tValue curve, derivativeAt tValue curve #)
  NewtonRaphson.Curve.solveFrom t1 evaluate
