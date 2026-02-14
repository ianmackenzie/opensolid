module OpenSolid.VectorCurve3D
  ( VectorCurve3D
  , Compiled
  , WithNoZeros (WithNoZeros)
  , WithNoInteriorZeros (WithNoInteriorZeros)
  , new
  , recursive
  , on
  , compiled
  , isZero
  , derivative
  , startValue
  , endValue
  , evaluate
  , evaluateAt
  , evaluateBounds
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
  , normalize
  , direction
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.WithNoZeros qualified as Curve1D.WithNoZeros
import OpenSolid.Direction3D (Direction3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import {-# SOURCE #-} OpenSolid.VectorCurve3D.WithNoInteriorZeros (WithNoInteriorZeros (WithNoInteriorZeros))
import {-# SOURCE #-} OpenSolid.VectorCurve3D.WithNoInteriorZeros qualified as VectorCurve3D.WithNoInteriorZeros
import {-# SOURCE #-} OpenSolid.VectorCurve3D.WithNoZeros (WithNoZeros (WithNoZeros))
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data VectorCurve3D units space = VectorCurve3D
  { compiled :: Compiled units space
  , derivative :: ~(VectorCurve3D units space)
  , maxSampledMagnitude :: ~(Quantity units)
  , nonZeroNormalized :: ~(VectorCurve3D Unitless space)
  }

type Compiled units space =
  CompiledFunction
    Number
    (Vector3D units space)
    (Interval Unitless)
    (VectorBounds3D units space)

instance HasUnits (VectorCurve3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3D units1 space1) (VectorCurve3D units2 space2)
  where
  coerce curve =
    VectorCurve3D
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , maxSampledMagnitude = Units.coerce curve.maxSampledMagnitude
      , nonZeroNormalized = curve.nonZeroNormalized
      }

instance ApproximateEquality (VectorCurve3D units space) units where
  curve1 ~= curve2 = do
    let equalValues t = evaluate curve1 t ~= evaluate curve2 t
    NonEmpty.allSatisfy equalValues Parameter.samples

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    units1
  where
  curve `intersects` vector = Tolerance.using (Quantity.squared_ ?tolerance) do
    squaredMagnitude_ (curve - vector) `intersects` Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    units1
  where
  vector `intersects` curve = curve `intersects` vector

instance Negation (VectorCurve3D units space) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (VectorCurve3D units space) (VectorCurve3D units space) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve3D units space) Sign (VectorCurve3D units space) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  curve + vector = curve + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  vector + curve = constant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  curve - vector = curve - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (Curve1D.compiled lhs ?*? rhs.compiled)
      (Curve1D.derivative lhs ?*? rhs + lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  c1 ?*? c2 = Curve1D.constant c1 ?*? c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Curve1D units2) (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? Curve1D.compiled rhs)
      (lhs.derivative ?*? rhs + lhs ?*? Curve1D.derivative rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Quantity units2) (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  curve ?*? value = curve ?*? Curve1D.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3D units1 space) (Quantity units2) (VectorCurve3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D (units1 ?/? units2) space)
  where
  curve ?/? value = Units.simplify (curve ?*? (1.0 ?/? value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve1D.new
      (lhs.compiled `dot_` rhs.compiled)
      (lhs.derivative `dot_` rhs + lhs `dot_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve3D units1 space1) (Vector3D units2 space2) (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  curve `dot_` vector = curve `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3D units1 space1) (VectorCurve3D units2 space2) (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  vector `dot_` curve = constant vector `dot_` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3D units space1) (Direction3D space2) (Curve1D units)
  where
  lhs `dot` rhs = lhs `dot` Vector3D.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3D space1) (VectorCurve3D units space2) (Curve1D units)
  where
  lhs `dot` rhs = Vector3D.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  lhs `cross_` rhs =
    new
      (lhs.compiled `cross_` rhs.compiled)
      (lhs.derivative `cross_` rhs + lhs `cross_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  curve `cross_` vector = curve `cross_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  vector `cross_` curve = constant vector `cross_` curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3D units space1)
    (Direction3D space2)
    (VectorCurve3D units space1)
  where
  lhs `cross` rhs = lhs `cross` Vector3D.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (VectorCurve3D units space2)
    (VectorCurve3D units space1)
  where
  lhs `cross` rhs = Vector3D.unit lhs `cross` rhs

instance
  Composition
    (VectorCurve3D units space)
    (Curve1D Unitless)
    (VectorCurve3D units space)
  where
  f . g = new (f.compiled . Curve1D.compiled g) ((f.derivative . g) * Curve1D.derivative g)

instance
  Composition
    (VectorCurve3D units space)
    (SurfaceFunction1D Unitless)
    (VectorSurfaceFunction3D units space)
  where
  curve . function =
    VectorSurfaceFunction3D.new
      (curve.compiled . function.compiled)
      (\p -> curve.derivative . function * SurfaceFunction1D.derivative p function)

compiled :: VectorCurve3D units space -> Compiled units space
compiled = (.compiled)

derivative :: VectorCurve3D units space -> VectorCurve3D units space
derivative = (.derivative)

isZero :: Tolerance units => VectorCurve3D units space -> Bool
isZero curve = curve.maxSampledMagnitude <= ?tolerance

transformBy :: Transform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector3D.transformBy transform)
          (VectorBounds3D.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

new :: Compiled units space -> VectorCurve3D units space -> VectorCurve3D units space
new givenCompiled givenDerivative = result
 where
  -- The test value to use to check if a curve is (likely) zero everywhere
  maxSampledMagnitude = NonEmpty.maximumOf (Vector3D.magnitude . evaluate result) Parameter.samples
  -- The normalized version of this curve, assuming it has no interior zeros
  nonZeroNormalized =
    result / VectorCurve3D.WithNoInteriorZeros.magnitude (WithNoInteriorZeros result)
  result =
    VectorCurve3D
      { compiled = givenCompiled
      , derivative = givenDerivative
      , maxSampledMagnitude
      , nonZeroNormalized
      }

recursive ::
  Compiled units space ->
  (VectorCurve3D units space -> VectorCurve3D units space) ->
  VectorCurve3D units space
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

zero :: VectorCurve3D units space
zero = constant Vector3D.zero

constant :: Vector3D units space -> VectorCurve3D units space
constant value = new (CompiledFunction.constant value) zero

on :: Plane3D global local -> VectorCurve2D units local -> VectorCurve3D units global
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
interpolateFrom v1 v2 = bezier (NonEmpty.two v1 v2)

arc ::
  Vector3D units space ->
  Vector3D units space ->
  Angle ->
  Angle ->
  VectorCurve3D units space
arc v1 v2 a b
  | v1 == Vector3D.zero && v2 == Vector3D.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = do
      let angle = Curve1D.interpolateFrom a b
      v1 * Curve1D.cos angle + v2 * Curve1D.sin angle

quadraticBezier ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorCurve3D units space
quadraticBezier v1 v2 v3 = bezier (NonEmpty.three v1 v2 v3)

cubicBezier ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorCurve3D units space
cubicBezier v1 v2 v3 v4 = bezier (NonEmpty.four v1 v2 v3 v4)

bezier :: NonEmpty (Vector3D units space) -> VectorCurve3D units space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezier (Bezier.derivative controlPoints))

startValue :: VectorCurve3D units space -> Vector3D units space
startValue curve = evaluate curve 0.0

endValue :: VectorCurve3D units space -> Vector3D units space
endValue curve = evaluate curve 1.0

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
desingularized start middle end = do
  let compiledDesingularized =
        CompiledFunction.desingularized
          (Curve1D.compiled Curve1D.t)
          start.compiled
          middle.compiled
          end.compiled
  let desingularizedDerivative = desingularized start.derivative middle.derivative end.derivative
  new compiledDesingularized desingularizedDerivative

evaluate :: VectorCurve3D units space -> Number -> Vector3D units space
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> VectorCurve3D units space -> Vector3D units space
evaluateAt tValue curve = evaluate curve tValue

evaluateBounds :: VectorCurve3D units space -> Interval Unitless -> VectorBounds3D units space
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

reverse :: VectorCurve3D units space -> VectorCurve3D units space
reverse curve = curve . (1.0 - Curve1D.t)

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
quotient_ lhs rhs =
  if rhs ~= Curve1D.zero
    then Error DivisionByZero
    else Ok (lhs ?/? Curve1D.WithNoInteriorZeros rhs)

instance
  Division_
    (VectorCurve3D units1 space)
    (Curve1D.WithNoZeros units2)
    (VectorCurve3D (units1 ?/? units2) space)
  where
  lhs ?/? rhsWithNoZeros = do
    let rhs = Curve1D.WithNoZeros.unwrap rhsWithNoZeros
    let compiledQuotient = compiled lhs ?/? Curve1D.compiled rhs
    let quotientDerivative = Units.simplify do
          (derivative lhs ?*? rhs - lhs ?*? Curve1D.derivative rhs)
            ?/? Curve1D.WithNoZeros.squared_ rhsWithNoZeros
    new compiledQuotient quotientDerivative

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve3D units1 space)
    (Curve1D.WithNoZeros units2)
    (VectorCurve3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve3D units1 space)
    (Curve1D.WithNoInteriorZeros units2)
    (VectorCurve3D (units1 ?/? units2) space)
  where
  (?/?) = VectorCurve.desingularizedQuotient

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve3D units1 space)
    (Curve1D.WithNoInteriorZeros units2)
    (VectorCurve3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

normalize :: Tolerance units => VectorCurve3D units space -> VectorCurve3D Unitless space
normalize curve = if isZero curve then zero else curve.nonZeroNormalized

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3D units1 space -> Curve1D units2
squaredMagnitude = VectorCurve.squaredMagnitude

squaredMagnitude_ :: VectorCurve3D units space -> Curve1D (units ?*? units)
squaredMagnitude_ curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector3D.squaredMagnitude_
          VectorBounds3D.squaredMagnitude_
          curve.compiled
  let squaredMagnitudeDerivative = 2.0 * curve `dot_` curve.derivative
  Curve1D.new compiledSquaredMagnitude squaredMagnitudeDerivative

magnitude :: Tolerance units => VectorCurve3D units space -> Curve1D units
magnitude = VectorCurve.magnitude

zeros :: Tolerance units => VectorCurve3D units space -> Result VectorCurve.IsZero (List Number)
zeros = VectorCurve.zeros

direction ::
  Tolerance units =>
  VectorCurve3D units space ->
  Result VectorCurve.IsZero (DirectionCurve3D space)
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
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame3D global local ->
  VectorCurve3D units global ->
  VectorCurve3D units local
relativeTo frame = placeIn (Frame3D.inverse frame)
