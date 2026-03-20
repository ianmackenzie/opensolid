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

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson2D qualified as NewtonRaphson2D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (IsDegenerate, Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvSpace (UvSpace)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorCurve qualified as VectorCurve
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D

data VectorCurve2D units space = VectorCurve2D
  { compiled :: Compiled units space
  , derivative :: ~(VectorCurve2D units space)
  , startValue :: ~(Vector2D units space)
  , endValue :: ~(Vector2D units space)
  , maxSampledMagnitude :: ~(Quantity units)
  }

type Compiled units space =
  CompiledFunction
    Number
    (Vector2D units space)
    (Interval Unitless)
    (VectorBounds2D units space)

instance FFI (VectorCurve2D Unitless FFI.Space) where
  representation = FFI.classRepresentation "VectorCurve2D"

instance FFI (VectorCurve2D Meters FFI.Space) where
  representation = FFI.classRepresentation "DisplacementCurve2D"

instance FFI (VectorCurve2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvVectorCurve"

instance HasUnits (VectorCurve2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2D units1 space1) (VectorCurve2D units2 space2)
  where
  coerce curve =
    VectorCurve2D
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startValue = Units.coerce curve.startValue
      , endValue = Units.coerce curve.endValue
      , maxSampledMagnitude = Units.coerce curve.maxSampledMagnitude
      }

instance HasUnits (Nondegenerate (VectorCurve2D units space)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nondegenerate (VectorCurve2D units1 space1))
    (Nondegenerate (VectorCurve2D units2 space2))
  where
  coerce (Nondegenerate curve) = Nondegenerate (Units.coerce curve)

instance HasUnits (Nonzero (VectorCurve2D units space)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nonzero (VectorCurve2D units1 space1))
    (Nonzero (VectorCurve2D units2 space2))
  where
  coerce (Nonzero curve) = Nonzero (Units.coerce curve)

instance ApproximateEquality (VectorCurve2D units space) (Tolerance units) where
  curve1 ~= curve2 = do
    let equalValuesAt t = value curve1 t ~= value curve2 t
    NonEmpty.all equalValuesAt Parameter.samples

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve2D units1 space1)
    (Vector2D units2 space2)
    (Tolerance units1)
  where
  curve `intersects` vector = Tolerance.using (Quantity.squared_ ?tolerance) do
    squaredMagnitude_ (curve - vector) `intersects` Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector2D units1 space1)
    (VectorCurve2D units2 space2)
    (Tolerance units1)
  where
  vector `intersects` curve = curve `intersects` vector

instance Negation (VectorCurve2D units space) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (VectorCurve2D units space) (VectorCurve2D units space) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve2D units space) Sign (VectorCurve2D units space) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2D units1 space1)
    (Vector2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  curve + vector = curve + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2D units1 space1)
    (VectorCurve2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  vector + curve = constant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2D units1 space1)
    (Vector2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  curve - vector = curve - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2D units1 space1)
    (VectorCurve2D units2 space2)
    (VectorCurve2D units1 space1)
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve1D units1)
    (VectorCurve2D units2 space)
    (VectorCurve2D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve2D units2 space)
    (VectorCurve2D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (Curve1D.compiled lhs ?*? rhs.compiled)
      (Curve1D.derivative lhs ?*? rhs + lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve2D units2 space) (VectorCurve2D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorCurve2D units2 space)
    (VectorCurve2D (units1 ?*? units2) space)
  where
  c1 ?*? c2 = Curve1D.constant c1 ?*? c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2D units1 space) (Curve1D units2) (VectorCurve2D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve2D units1 space)
    (Curve1D units2)
    (VectorCurve2D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? Curve1D.compiled rhs)
      (lhs.derivative ?*? rhs + lhs ?*? Curve1D.derivative rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2D units1 space) (Quantity units2) (VectorCurve2D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2D units1 space) (Quantity units2) (VectorCurve2D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve2D units1 space)
    (Quantity units2)
    (VectorCurve2D (units1 ?/? units2) space)
  where
  curve ?/? quantity = Units.simplify (curve ?*? (1.0 ?/? quantity))

instance
  Multiplication_
    (VectorCurve2D units1 space)
    (Quantity units2)
    (VectorCurve2D (units1 ?*? units2) space)
  where
  curve ?*? quantity = curve ?*? Curve1D.constant quantity

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve1D.new
      (lhs.compiled `dot_` rhs.compiled)
      (lhs.derivative `dot_` rhs + lhs `dot_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve2D units1 space1) (Vector2D units2 space2) (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve2D units1 space1)
    (Vector2D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  curve `dot_` vector = curve `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2D units1 space1) (VectorCurve2D units2 space2) (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  vector `dot_` curve = constant vector `dot_` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2D units space1) (Direction2D space2) (Curve1D units)
  where
  lhs `dot` rhs = lhs `dot` Vector2D.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2D space1) (VectorCurve2D units space2) (Curve1D units)
  where
  lhs `dot` rhs = Vector2D.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `cross_` rhs =
    Curve1D.new
      (lhs.compiled `cross_` rhs.compiled)
      (lhs.derivative `cross_` rhs + lhs `cross_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2D units1 space1)
    (Vector2D units2 space2)
    (Curve1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve2D units1 space1)
    (Vector2D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  curve `cross_` vector = curve `cross_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  vector `cross_` curve = constant vector `cross_` curve

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2D units space1) (Direction2D space2) (Curve1D units)
  where
  lhs `cross` rhs = lhs `cross` Vector2D.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2D space1) (VectorCurve2D units space2) (Curve1D units)
  where
  lhs `cross` rhs = Vector2D.unit lhs `cross` rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)
  where
  point + curve = Curve2D.constant point + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)
  where
  point - curve = Curve2D.constant point - curve

instance
  Composition
    (VectorCurve2D units space)
    (Curve1D Unitless)
    (VectorCurve2D units space)
  where
  f . g =
    new
      (f.compiled . Curve1D.compiled g)
      ((f.derivative . g) * Curve1D.derivative g)

instance
  Composition
    (VectorCurve2D units space)
    (SurfaceFunction1D Unitless)
    (VectorSurfaceFunction2D units space)
  where
  curve . function =
    VectorSurfaceFunction2D.new
      (curve.compiled . function.compiled)
      (\p -> (curve.derivative . function) * SurfaceFunction1D.derivative p function)

instance
  Composition
    (VectorCurve2D units space)
    SurfaceParameter
    (VectorSurfaceFunction2D units space)
  where
  curve . parameter = curve . SurfaceFunction1D.parameter parameter

compiled :: VectorCurve2D units space -> Compiled units space
compiled = (.compiled)

derivative :: VectorCurve2D units space -> VectorCurve2D units space
derivative = (.derivative)

nondegenerate ::
  Tolerance units =>
  VectorCurve2D units space ->
  Result IsDegenerate (Nondegenerate (VectorCurve2D units space))
nondegenerate = VectorCurve.nondegenerate

transformBy ::
  Transform2D tag translationUnits space ->
  VectorCurve2D units space ->
  VectorCurve2D units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Vector2D.transformBy transform)
          (VectorBounds2D.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

rotateBy ::
  forall units space.
  Angle ->
  VectorCurve2D units space ->
  VectorCurve2D units space
rotateBy angle = transformBy (Transform2D.rotateAround (Point2D.origin @units @space) angle)

new :: Compiled units space -> VectorCurve2D units space -> VectorCurve2D units space
new givenCompiled givenDerivative = result
 where
  -- The test value to use to check if a curve is (likely) zero everywhere
  maxSampledMagnitude = NonEmpty.maximumOf (Vector2D.magnitude . value result) Parameter.samples
  result =
    VectorCurve2D
      { compiled = givenCompiled
      , derivative = givenDerivative
      , startValue = CompiledFunction.value givenCompiled 0.0
      , endValue = CompiledFunction.value givenCompiled 1.0
      , maxSampledMagnitude
      }

-- | The constant zero vector.
zero :: VectorCurve2D units space
zero = constant Vector2D.zero

-- | Create a curve with a constant value.
constant :: Vector2D units space -> VectorCurve2D units space
constant vector = new (CompiledFunction.constant vector) zero

unit :: DirectionCurve2D space -> VectorCurve2D Unitless space
unit = DirectionCurve2D.unwrap

-- | Create a curve from its X and Y component curves.
xy :: forall units space. Curve1D units -> Curve1D units -> VectorCurve2D units space
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

interpolateFrom :: Vector2D units space -> Vector2D units space -> VectorCurve2D units space
interpolateFrom v1 v2 = bezier (NonEmpty.two v1 v2)

arc ::
  Vector2D units space ->
  Vector2D units space ->
  Angle ->
  Angle ->
  VectorCurve2D units space
arc v1 v2 a b
  | v1 == Vector2D.zero && v2 == Vector2D.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = do
      let angle = Curve1D.interpolateFrom a b
      v1 * Curve1D.cos angle + v2 * Curve1D.sin angle

quadraticBezier ::
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  VectorCurve2D units space
quadraticBezier v1 v2 v3 = bezier (NonEmpty.three v1 v2 v3)

cubicBezier ::
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  VectorCurve2D units space
cubicBezier v1 v2 v3 v4 = bezier (NonEmpty.four v1 v2 v3 v4)

bezier :: NonEmpty (Vector2D units space) -> VectorCurve2D units space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezier (Bezier.derivative controlPoints))

startValue :: VectorCurve2D units space -> Vector2D units space
startValue = (.startValue)

endValue :: VectorCurve2D units space -> Vector2D units space
endValue = (.endValue)

desingularize ::
  Maybe (Vector2D units space, Vector2D units space) ->
  VectorCurve2D units space ->
  Maybe (Vector2D units space, Vector2D units space) ->
  VectorCurve2D units space
desingularize = VectorCurve.desingularize

desingularized ::
  VectorCurve2D units space ->
  VectorCurve2D units space ->
  VectorCurve2D units space ->
  VectorCurve2D units space
desingularized start middle end = do
  let compiledDesingularized =
        CompiledFunction.desingularized
          (Curve1D.compiled Curve1D.t)
          start.compiled
          middle.compiled
          end.compiled
  let desingularizedDerivative = desingularized start.derivative middle.derivative end.derivative
  new compiledDesingularized desingularizedDerivative

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
value :: VectorCurve2D units space -> Number -> Vector2D units space
value curve 0.0 = curve.startValue
value curve 1.0 = curve.endValue
value curve tValue = CompiledFunction.value curve.compiled tValue

bounds :: VectorCurve2D units space -> Interval Unitless -> VectorBounds2D units space
bounds curve tBounds = CompiledFunction.bounds curve.compiled tBounds

{-# INLINE derivativeValue #-}
derivativeValue :: VectorCurve2D units space -> Number -> Vector2D units space
derivativeValue = VectorCurve.derivativeValue

{-# INLINE derivativeBounds #-}
derivativeBounds :: VectorCurve2D units space -> Interval Unitless -> VectorBounds2D units space
derivativeBounds = VectorCurve.derivativeBounds

{-# INLINE secondDerivativeValue #-}
secondDerivativeValue :: VectorCurve2D units space -> Number -> Vector2D units space
secondDerivativeValue = VectorCurve.secondDerivativeValue

{-# INLINE secondDerivativeBounds #-}
secondDerivativeBounds ::
  VectorCurve2D units space ->
  Interval Unitless ->
  VectorBounds2D units space
secondDerivativeBounds = VectorCurve.secondDerivativeBounds

-- | Get the X coordinate of a 2D curve as a scalar curve.
xComponent :: VectorCurve2D units space -> Curve1D units
xComponent curve = do
  let compiledXComponent =
        CompiledFunction.map
          Expression.xComponent
          Vector2D.xComponent
          VectorBounds2D.xComponent
          curve.compiled
  Curve1D.new compiledXComponent (xComponent curve.derivative)

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yComponent :: VectorCurve2D units space -> Curve1D units
yComponent curve = do
  let compiledYComponent =
        CompiledFunction.map
          Expression.yComponent
          Vector2D.yComponent
          VectorBounds2D.yComponent
          curve.compiled
  Curve1D.new compiledYComponent (yComponent curve.derivative)

components :: VectorCurve2D units space -> (Curve1D units, Curve1D units)
components curve = (xComponent curve, yComponent curve)

reverse :: VectorCurve2D units space -> VectorCurve2D units space
reverse curve = curve . (1.0 - Curve1D.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve2D units3 space)
quotient lhs rhs = Result.map Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorCurve2D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve2D (units1 ?/? units2) space)
quotient_ lhs rhs =
  if rhs ~= Curve1D.zero
    then Error DivisionByZero
    else Ok (lhs ?/? Nondegenerate rhs)

instance
  Division_
    (VectorCurve2D units1 space)
    (Nonzero (Curve1D units2))
    (VectorCurve2D (units1 ?/? units2) space)
  where
  lhs ?/? Nonzero rhs = do
    let compiledQuotient = compiled lhs ?/? Curve1D.compiled rhs
    let quotientDerivative = Units.simplify do
          (derivative lhs ?*? rhs - lhs ?*? Curve1D.derivative rhs)
            ?/? Curve1D.Nonzero.squared_ (Nonzero rhs)
    new compiledQuotient quotientDerivative

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve2D units1 space)
    (Nonzero (Curve1D units2))
    (VectorCurve2D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve2D units1 space)
    (Nondegenerate (Curve1D units2))
    (VectorCurve2D (units1 ?/? units2) space)
  where
  (?/?) = VectorCurve.desingularizedQuotient

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve2D units1 space)
    (Nondegenerate (Curve1D units2))
    (VectorCurve2D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2D units1 space -> Curve1D units2
squaredMagnitude = VectorCurve.squaredMagnitude

squaredMagnitude_ :: VectorCurve2D units space -> Curve1D (units ?*? units)
squaredMagnitude_ curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector2D.squaredMagnitude_
          VectorBounds2D.squaredMagnitude_
          curve.compiled
  let squaredMagnitudeDerivative = 2.0 * curve `dot_` derivative curve
  Curve1D.new compiledSquaredMagnitude squaredMagnitudeDerivative

magnitude :: Tolerance units => VectorCurve2D units space -> Curve1D units
magnitude = VectorCurve.magnitude

isZero :: Tolerance units => VectorCurve2D units space -> Bool
isZero curve = curve.maxSampledMagnitude <= ?tolerance

singularityTolerance :: VectorCurve2D units space -> Quantity units
singularityTolerance curve = Tolerance.unitless * curve.maxSampledMagnitude

singular0 :: VectorCurve2D units space -> Bool
singular0 curve =
  Tolerance.using (singularityTolerance curve) (curve.startValue ~= Vector2D.zero)

singular1 :: VectorCurve2D units space -> Bool
singular1 curve =
  Tolerance.using (singularityTolerance curve) (curve.endValue ~= Vector2D.zero)

zeros :: Tolerance units => VectorCurve2D units space -> Result IsDegenerate (List Number)
zeros = VectorCurve.zeros

direction ::
  Tolerance units =>
  VectorCurve2D units space ->
  Result IsDegenerate (DirectionCurve2D space)
direction = VectorCurve.direction

placeIn ::
  Frame2D frameUnits global local ->
  VectorCurve2D units local ->
  VectorCurve2D units global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Vector2D.placeIn frame)
          (VectorBounds2D.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame2D frameUnits global local ->
  VectorCurve2D units global ->
  VectorCurve2D units local
relativeTo frame = placeIn (Frame2D.inverse frame)

placeOn ::
  Plane3D global local ->
  VectorCurve2D units local ->
  VectorCurve3D units global
placeOn plane curve = VectorCurve3D.on plane curve

convert ::
  Quantity (units2 ?/? units1) ->
  VectorCurve2D units1 space ->
  VectorCurve2D units2 space
convert factor curve = Units.simplify (curve ?*? factor)

unconvert ::
  Quantity (units2 ?/? units1) ->
  VectorCurve2D units2 space ->
  VectorCurve2D units1 space
unconvert factor curve = Units.simplify (curve ?/? factor)

newtonRaphson :: VectorCurve2D units space -> Number -> Number
newtonRaphson curve t1 = do
  let evaluate tValue = (# value curve tValue, derivativeValue curve tValue #)
  NewtonRaphson2D.curve evaluate t1
