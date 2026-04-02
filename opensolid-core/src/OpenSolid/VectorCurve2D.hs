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
  , value
  , bounds
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , xComponent
  , yComponent
  , components
  , zero
  , constant
  , unit
  , xy
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
  , isZero
  , singular0
  , singular1
  , zeros
  , direction
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

import Data.Void (Void)
import OpenSolid.Angle (Angle)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson2D qualified as NewtonRaphson2D
import OpenSolid.Nondegenerate (IsDegenerate, Nondegenerate)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D

type VectorCurve2D units = VectorCurve 2 units Void

type Compiled units = VectorCurve.Compiled 2 units Void

compiled :: VectorCurve2D units -> Compiled units
compiled = VectorCurve.compiled

derivative :: VectorCurve2D units -> VectorCurve2D units
derivative = VectorCurve.derivative

nondegenerate ::
  Tolerance units =>
  VectorCurve2D units ->
  Result IsDegenerate (Nondegenerate (VectorCurve2D units))
nondegenerate = VectorCurve.nondegenerate

transformBy ::
  Transform2D tag translationUnits ->
  VectorCurve2D units ->
  VectorCurve2D units
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector2D.transformBy transform)
          (VectorBounds2D.transformBy transform)
          (compiled curve)
  new compiledTransformed (transformBy transform (derivative curve))

rotateBy :: Angle -> VectorCurve2D units -> VectorCurve2D units
rotateBy angle = transformBy (Transform2D.rotateAround Point2D.origin angle)

new :: Compiled units -> VectorCurve2D units -> VectorCurve2D units
new = VectorCurve.new

-- | The constant zero vector.
zero :: VectorCurve2D units
zero = VectorCurve.zero

-- | Create a curve with a constant value.
constant :: Vector2D units -> VectorCurve2D units
constant = VectorCurve.constant

unit :: DirectionCurve2D -> VectorCurve2D Unitless
unit = DirectionCurve2D.unwrap

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

startValue :: VectorCurve2D units -> Vector2D units
startValue = VectorCurve.startValue

endValue :: VectorCurve2D units -> Vector2D units
endValue = VectorCurve.endValue

desingularize ::
  Maybe (Vector2D units, Vector2D units) ->
  VectorCurve2D units ->
  Maybe (Vector2D units, Vector2D units) ->
  VectorCurve2D units
desingularize = VectorCurve.desingularize

desingularized ::
  VectorCurve2D units ->
  VectorCurve2D units ->
  VectorCurve2D units ->
  VectorCurve2D units
desingularized = VectorCurve.desingularized

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
value :: VectorCurve2D units -> Number -> Vector2D units
value = VectorCurve.value

bounds :: VectorCurve2D units -> Interval Unitless -> VectorBounds2D units
bounds = VectorCurve.bounds

{-# INLINE derivativeValue #-}
derivativeValue :: VectorCurve2D units -> Number -> Vector2D units
derivativeValue = VectorCurve.derivativeValue

{-# INLINE derivativeBounds #-}
derivativeBounds :: VectorCurve2D units -> Interval Unitless -> VectorBounds2D units
derivativeBounds = VectorCurve.derivativeBounds

{-# INLINE secondDerivativeValue #-}
secondDerivativeValue :: VectorCurve2D units -> Number -> Vector2D units
secondDerivativeValue = VectorCurve.secondDerivativeValue

{-# INLINE secondDerivativeBounds #-}
secondDerivativeBounds ::
  VectorCurve2D units ->
  Interval Unitless ->
  VectorBounds2D units
secondDerivativeBounds = VectorCurve.secondDerivativeBounds

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

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2D units1 ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve2D units3)
quotient lhs rhs = Result.map Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorCurve2D units1 ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve2D (units1 ?/? units2))
quotient_ = VectorCurve.quotient_

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2D units1 -> Curve1D units2
squaredMagnitude = VectorCurve.squaredMagnitude

squaredMagnitude_ :: VectorCurve2D units -> Curve1D (units ?*? units)
squaredMagnitude_ = VectorCurve.squaredMagnitude_

magnitude :: Tolerance units => VectorCurve2D units -> Curve1D units
magnitude = VectorCurve.magnitude

isZero :: Tolerance units => VectorCurve2D units -> Bool
isZero = VectorCurve.isZero

singular0 :: VectorCurve2D units -> Bool
singular0 = VectorCurve.singular0

singular1 :: VectorCurve2D units -> Bool
singular1 = VectorCurve.singular1

zeros :: Tolerance units => VectorCurve2D units -> Result IsDegenerate (List Number)
zeros = VectorCurve.zeros

direction :: Tolerance units => VectorCurve2D units -> Result IsDegenerate DirectionCurve2D
direction = VectorCurve.direction

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

newtonRaphson :: VectorCurve2D units -> Number -> Number
newtonRaphson curve t1 = do
  let evaluate tValue = (# value curve tValue, derivativeValue curve tValue #)
  NewtonRaphson2D.curve evaluate t1
