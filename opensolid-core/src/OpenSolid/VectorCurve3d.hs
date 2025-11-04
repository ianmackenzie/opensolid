module OpenSolid.VectorCurve3d
  ( VectorCurve3d
  , Compiled
  , new
  , recursive
  , on
  , compiled
  , derivative
  , startValue
  , endValue
  , evaluate
  , evaluateAt
  , evaluateBounds
  , zero
  , constant
  , line
  , arc
  , quadraticBezier
  , cubicBezier
  , bezier
  , desingularize
  , desingularized
  , quotient
  , quotient'
  , unsafeQuotient
  , unsafeQuotient'
  , magnitude
  , squaredMagnitude
  , squaredMagnitude'
  , reverse
  , IsZero (IsZero)
  , zeros
  , HasZero (HasZero)
  , direction
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Direction3d (Direction3d)
import {-# SOURCE #-} OpenSolid.DirectionCurve3d (DirectionCurve3d)
import {-# SOURCE #-} OpenSolid.DirectionCurve3d qualified as DirectionCurve3d
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Expression.VectorCurve3d qualified as Expression.VectorCurve3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data VectorCurve3d (coordinateSystem :: CoordinateSystem)
  = VectorCurve3d (Compiled coordinateSystem) ~(VectorCurve3d coordinateSystem)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Number
    (Vector3d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds3d coordinateSystem)

instance HasField "compiled" (VectorCurve3d (space @ units)) (Compiled (space @ units)) where
  getField = compiled

instance HasField "derivative" (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units)) where
  getField = derivative

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve3d (space @ units1)) (Curve units2)
  where
  getField = squaredMagnitude

instance
  HasField
    "squaredMagnitude'"
    (VectorCurve3d (space @ units))
    (Curve (units *# units))
  where
  getField = squaredMagnitude'

instance HasUnits (VectorCurve3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d (space1 @ units1)) (VectorCurve3d (space2 @ units2))
  where
  coerce curve = VectorCurve3d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    units1
  where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    units1
  where
  curve ~= vector = List.allTrue [evaluate curve t ~= vector | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    units1
  where
  curve ^ vector = Tolerance.using Tolerance.squared' do
    (curve - vector).squaredMagnitude' ^ Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    units1
  where
  vector ^ curve = curve ^ vector

instance Negation (VectorCurve3d (space @ units)) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units)) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units)) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  curve + vector = curve + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  vector + curve = constant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  curve - vector = curve - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication'
    (Curve units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 *# units2)))
  where
  lhs *# rhs =
    new
      @ lhs.compiled *# rhs.compiled
      @ lhs.derivative *# rhs + lhs *# rhs.derivative

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication'
    (Quantity units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 *# units2)))
  where
  c1 *# c2 = Curve.constant c1 *# c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 *# units2)))
  where
  lhs *# rhs =
    new
      (lhs.compiled *# rhs.compiled)
      (lhs.derivative *# rhs + lhs *# rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Quantity units2) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs *# rhs)

instance
  Multiplication'
    (VectorCurve3d (space @ units1))
    (Quantity units2)
    (VectorCurve3d (space @ (units1 *# units2)))
  where
  curve *# value = curve *# Curve.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Quantity units2) (VectorCurve3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs /# rhs)

instance
  Division'
    (VectorCurve3d (space @ units1))
    (Quantity units2)
    (VectorCurve3d (space @ (units1 /# units2)))
  where
  curve /# value = Units.simplify (curve *# (1.0 /# value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 *# units2))
  where
  lhs `dot'` rhs =
    Curve.new
      (lhs.compiled `dot'` rhs.compiled)
      (lhs.derivative `dot'` rhs + lhs `dot'` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve3d (space1 @ units1)) (Vector3d (space2 @ units2)) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Curve (units1 *# units2))
  where
  curve `dot'` vector = curve `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d (space1 @ units1)) (VectorCurve3d (space2 @ units2)) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 *# units2))
  where
  vector `dot'` curve = constant vector `dot'` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (Direction3d space2) (Curve units)
  where
  lhs `dot` rhs = lhs `dot` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (VectorCurve3d (space2 @ units)) (Curve units)
  where
  lhs `dot` rhs = Vector3d.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 *# units2)))
  where
  lhs `cross'` rhs =
    new
      (lhs.compiled `cross'` rhs.compiled)
      (lhs.derivative `cross'` rhs + lhs `cross'` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 *# units2)))
  where
  curve `cross'` vector = curve `cross'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 *# units2)))
  where
  vector `cross'` curve = constant vector `cross'` curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (Direction3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs `cross` rhs = lhs `cross` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  lhs `cross` rhs = Vector3d.unit lhs `cross` rhs

instance
  Composition
    (Curve Unitless)
    (VectorCurve3d (space @ units))
    (VectorCurve3d (space @ units))
  where
  f . g = new (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  curve . function =
    VectorSurfaceFunction3d.new
      @ curve.compiled . function.compiled
      @ \p -> curve.derivative . function * SurfaceFunction.derivative p function

compiled :: VectorCurve3d (space @ units) -> Compiled (space @ units)
compiled (VectorCurve3d c _) = c

derivative :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
derivative (VectorCurve3d _ d) = d

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.VectorCurve3d.transformBy transform)
          (Vector3d.transformBy transform)
          (VectorBounds3d.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

new :: Compiled (space @ units) -> VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
new = VectorCurve3d

recursive ::
  Compiled (space @ units) ->
  (VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)) ->
  VectorCurve3d (space @ units)
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

zero :: VectorCurve3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
constant value = new (CompiledFunction.constant value) zero

on ::
  Plane3d (space @ planeUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve3d (space @ units)
on plane vectorCurve2d = do
  let compiledPlanar =
        CompiledFunction.map
          (Expression.VectorCurve2d.placeOn plane)
          (Vector2d.placeOn plane)
          (VectorBounds2d.placeOn plane)
          vectorCurve2d.compiled
  new compiledPlanar (on plane vectorCurve2d.derivative)

line :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorCurve3d (space @ units)
line v1 v2 = bezier (NonEmpty.two v1 v2)

arc ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Angle ->
  Angle ->
  VectorCurve3d (space @ units)
arc v1 v2 a b
  | v1 == Vector3d.zero && v2 == Vector3d.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = do
      let angle = Curve.line a b
      v1 * Curve.cos angle + v2 * Curve.sin angle

quadraticBezier ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorCurve3d (space @ units)
quadraticBezier v1 v2 v3 = bezier (NonEmpty.three v1 v2 v3)

cubicBezier ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorCurve3d (space @ units)
cubicBezier v1 v2 v3 v4 = bezier (NonEmpty.four v1 v2 v3 v4)

bezier :: NonEmpty (Vector3d (space @ units)) -> VectorCurve3d (space @ units)
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezier (Bezier.derivative controlPoints))

startValue :: VectorCurve3d (space @ units) -> Vector3d (space @ units)
startValue curve = evaluate curve 0.0

endValue :: VectorCurve3d (space @ units) -> Vector3d (space @ units)
endValue curve = evaluate curve 1.0

desingularize ::
  Maybe (Vector3d (space @ units), Vector3d (space @ units)) ->
  VectorCurve3d (space @ units) ->
  Maybe (Vector3d (space @ units), Vector3d (space @ units)) ->
  VectorCurve3d (space @ units)
desingularize Nothing curve Nothing = curve
desingularize startSingularity curve endSingularity = do
  let startCurve = case startSingularity of
        Nothing -> curve
        Just (value0, firstDerivative0) -> do
          let t0 = Desingularization.t0
          let valueT0 = evaluate curve t0
          let firstDerivativeT0 = evaluate curve.derivative t0
          let secondDerivativeT0 = evaluate curve.derivative.derivative t0
          bezier $
            Bezier.syntheticStart
              value0
              firstDerivative0
              valueT0
              firstDerivativeT0
              secondDerivativeT0
  let endCurve = case endSingularity of
        Nothing -> curve
        Just (value1, firstDerivative1) -> do
          let t1 = Desingularization.t1
          let valueT1 = evaluate curve t1
          let firstDerivativeT1 = evaluate curve.derivative t1
          let secondDerivativeT1 = evaluate curve.derivative.derivative t1
          bezier $
            Bezier.syntheticEnd
              valueT1
              firstDerivativeT1
              secondDerivativeT1
              value1
              firstDerivative1
  desingularized startCurve curve endCurve

desingularized ::
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
desingularized start middle end =
  new
    @ CompiledFunction.desingularized Curve.t.compiled start.compiled middle.compiled end.compiled
    @ desingularized start.derivative middle.derivative end.derivative

evaluate :: VectorCurve3d (space @ units) -> Number -> Vector3d (space @ units)
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> VectorCurve3d (space @ units) -> Vector3d (space @ units)
evaluateAt tValue curve = evaluate curve tValue

evaluateBounds :: VectorCurve3d (space @ units) -> Bounds Unitless -> VectorBounds3d (space @ units)
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

reverse :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d (space @ units3))
quotient lhs rhs = Units.specialize (quotient' lhs rhs)

quotient' ::
  Tolerance units2 =>
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d (space @ (units1 /# units2)))
quotient' numerator denominator =
  if denominator ~= Quantity.zero
    then Failure DivisionByZero
    else Success do
      let singularity0 =
            if Curve.evaluate denominator 0.0 ~= Quantity.zero
              then Just (lhopital numerator denominator 0.0)
              else Nothing
      let singularity1 =
            if Curve.evaluate denominator 1.0 ~= Quantity.zero
              then Just (lhopital numerator denominator 1.0)
              else Nothing
      desingularize singularity0 (unsafeQuotient' numerator denominator) singularity1

lhopital ::
  Tolerance units2 =>
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  Number ->
  (Vector3d (space @ (units1 /# units2)), Vector3d (space @ (units1 /# units2)))
lhopital numerator denominator tValue = do
  let numerator' = evaluate numerator.derivative tValue
  let numerator'' = evaluate numerator.derivative.derivative tValue
  let denominator' = Curve.evaluate denominator.derivative tValue
  let denominator'' = Curve.evaluate denominator.derivative.derivative tValue
  let value = numerator' /# denominator'
  let firstDerivative =
        Units.simplify $
          (numerator'' *# denominator' - numerator' *# denominator'')
            /# (2.0 * Quantity.squared' denominator')
  (value, firstDerivative)

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  VectorCurve3d (space @ units3)
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient' numerator denominator)

unsafeQuotient' ::
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  VectorCurve3d (space @ (units1 /# units2))
unsafeQuotient' numerator denominator = do
  new
    @ numerator.compiled /# denominator.compiled
    @ Units.simplify do
      unsafeQuotient'
        (numerator.derivative *# denominator - numerator *# denominator.derivative)
        (Curve.squared' denominator)

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d (space @ units1) -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve3d (space @ units) -> Curve (units *# units)
squaredMagnitude' curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3d.squaredMagnitude'
          Vector3d.squaredMagnitude'
          VectorBounds3d.squaredMagnitude'
          curve.compiled
  let squaredMagnitudeDerivative = 2.0 * curve `dot'` curve.derivative
  Curve.new compiledSquaredMagnitude squaredMagnitudeDerivative

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve3d (space @ units) -> Curve units
magnitude curve = Curve.sqrt' (squaredMagnitude' curve)

data IsZero = IsZero deriving (Eq, Show, Error.Message)

zeros :: Tolerance units => VectorCurve3d (space @ units) -> Result IsZero (List Number)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve.zeros (squaredMagnitude' curve)) of
    Success zeros1d -> Success (List.map (.location) zeros1d)
    Failure Curve.IsZero -> Failure IsZero

direction ::
  Tolerance units =>
  VectorCurve3d (space @ units) ->
  Result IsZero (DirectionCurve3d space)
direction curve = case quotient curve (magnitude curve) of
  Failure DivisionByZero -> Failure IsZero
  Success normalizedCurve -> Success (DirectionCurve3d.unsafe normalizedCurve)

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorCurve3d (local @ units) ->
  VectorCurve3d (global @ units)
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve3d.placeIn frame)
          (Vector3d.placeIn frame)
          (VectorBounds3d.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorCurve3d (global @ units) ->
  VectorCurve3d (local @ units)
relativeTo frame = placeIn (Frame3d.inverse frame)
