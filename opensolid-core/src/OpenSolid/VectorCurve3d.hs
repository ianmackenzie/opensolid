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
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  , magnitude
  , squaredMagnitude
  , squaredMagnitude_
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

import GHC.Records (HasField (getField))
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
import OpenSolid.Units (HasUnits)
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

data VectorCurve3d space units
  = VectorCurve3d (Compiled space units) ~(VectorCurve3d space units)

type Compiled space units =
  CompiledFunction
    Number
    (Vector3d space units)
    (Bounds Unitless)
    (VectorBounds3d space units)

instance HasField "compiled" (VectorCurve3d space units) (Compiled space units) where
  getField = compiled

instance HasField "derivative" (VectorCurve3d space units) (VectorCurve3d space units) where
  getField = derivative

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve3d space units1) (Curve units2)
  where
  getField = squaredMagnitude

instance
  HasField
    "squaredMagnitude_"
    (VectorCurve3d space units)
    (Curve (units ?*? units))
  where
  getField = squaredMagnitude_

instance HasUnits (VectorCurve3d space units) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d space1 units1) (VectorCurve3d space2 units2)
  where
  coerce curve = VectorCurve3d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance ApproximateEquality (VectorCurve3d space units) units where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve3d space1 units1)
    (Vector3d space2 units2)
    units1
  where
  curve `intersects` vector = Tolerance.using (Quantity.squared_ ?tolerance) do
    (curve .-. vector).squaredMagnitude_ `intersects` Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector3d space1 units1)
    (VectorCurve3d space2 units2)
    units1
  where
  vector `intersects` curve = curve `intersects` vector

instance Negation (VectorCurve3d space units) where
  negative curve = new (negative curve.compiled) (negative curve.derivative)

instance Multiplication Sign (VectorCurve3d space units) (VectorCurve3d space units) where
  Positive .*. curve = curve
  Negative .*. curve = negative curve

instance Multiplication (VectorCurve3d space units) Sign (VectorCurve3d space units) where
  curve .*. Positive = curve
  curve .*. Negative = negative curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 units1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (lhs.derivative .+. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3d space1 units1)
    (Vector3d space2 units2)
    (VectorCurve3d space1 units1)
  where
  curve .+. vector = curve .+. constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 units1)
  where
  vector .+. curve = constant vector .+. curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 units1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3d space1 units1)
    (Vector3d space2 units2)
    (VectorCurve3d space1 units1)
  where
  curve .-. vector = curve .-. constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 units1)
  where
  vector .-. curve = constant vector .-. curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d space units2) (VectorCurve3d space units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve units1)
    (VectorCurve3d space units2)
    (VectorCurve3d space (units1 ?*? units2))
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (lhs.derivative ?*? rhs .+. lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve3d space units2) (VectorCurve3d space units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorCurve3d space units2)
    (VectorCurve3d space (units1 ?*? units2))
  where
  c1 ?*? c2 = Curve.constant c1 ?*? c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d space units1) (Curve units2) (VectorCurve3d space units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve3d space units1)
    (Curve units2)
    (VectorCurve3d space (units1 ?*? units2))
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (lhs.derivative ?*? rhs .+. lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d space units1) (Quantity units2) (VectorCurve3d space units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve3d space units1)
    (Quantity units2)
    (VectorCurve3d space (units1 ?*? units2))
  where
  curve ?*? value = curve ?*? Curve.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d space units1) (Quantity units2) (VectorCurve3d space units3)
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve3d space units1)
    (Quantity units2)
    (VectorCurve3d space (units1 ?/? units2))
  where
  curve ?/? value = Units.simplify (curve ?*? (1 /? value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (Curve (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve.new
      (lhs.compiled `dot_` rhs.compiled)
      (lhs.derivative `dot_` rhs .+. lhs `dot_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve3d space1 units1) (Vector3d space2 units2) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3d space1 units1)
    (Vector3d space2 units2)
    (Curve (units1 ?*? units2))
  where
  curve `dot_` vector = curve `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d space1 units1) (VectorCurve3d space2 units2) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector3d space1 units1)
    (VectorCurve3d space2 units2)
    (Curve (units1 ?*? units2))
  where
  vector `dot_` curve = constant vector `dot_` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d space1 units) (Direction3d space2) (Curve units)
  where
  lhs `dot` rhs = lhs `dot` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (VectorCurve3d space2 units) (Curve units)
  where
  lhs `dot` rhs = Vector3d.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 (units1 ?*? units2))
  where
  lhs `cross_` rhs =
    new
      (lhs.compiled `cross_` rhs.compiled)
      (lhs.derivative `cross_` rhs .+. lhs `cross_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d space1 units1)
    (Vector3d space2 units2)
    (VectorCurve3d space1 units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3d space1 units1)
    (Vector3d space2 units2)
    (VectorCurve3d space1 (units1 ?*? units2))
  where
  curve `cross_` vector = curve `cross_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector3d space1 units1)
    (VectorCurve3d space2 units2)
    (VectorCurve3d space1 (units1 ?*? units2))
  where
  vector `cross_` curve = constant vector `cross_` curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d space1 units)
    (Direction3d space2)
    (VectorCurve3d space1 units)
  where
  lhs `cross` rhs = lhs `cross` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorCurve3d space2 units)
    (VectorCurve3d space1 units)
  where
  lhs `cross` rhs = Vector3d.unit lhs `cross` rhs

instance
  Composition
    (Curve Unitless)
    (VectorCurve3d space units)
    (VectorCurve3d space units)
  where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve3d space units)
    (VectorSurfaceFunction3d space units)
  where
  curve `compose` function =
    VectorSurfaceFunction3d.new
      (curve.compiled `compose` function.compiled)
      (\p -> curve.derivative `compose` function .*. SurfaceFunction.derivative p function)

compiled :: VectorCurve3d space units -> Compiled space units
compiled (VectorCurve3d c _) = c

derivative :: VectorCurve3d space units -> VectorCurve3d space units
derivative (VectorCurve3d _ d) = d

transformBy ::
  Transform3d tag space translationUnits ->
  VectorCurve3d space units ->
  VectorCurve3d space units
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.VectorCurve3d.transformBy transform)
          (Vector3d.transformBy transform)
          (VectorBounds3d.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

new :: Compiled space units -> VectorCurve3d space units -> VectorCurve3d space units
new = VectorCurve3d

recursive ::
  Compiled space units ->
  (VectorCurve3d space units -> VectorCurve3d space units) ->
  VectorCurve3d space units
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

zero :: VectorCurve3d space units
zero = constant Vector3d.zero

constant :: Vector3d space units -> VectorCurve3d space units
constant value = new (CompiledFunction.constant value) zero

on ::
  Plane3d space planeUnits (Defines local) ->
  VectorCurve2d local units ->
  VectorCurve3d space units
on plane vectorCurve2d = do
  let compiledPlanar =
        CompiledFunction.map
          (Expression.VectorCurve2d.placeOn plane)
          (Vector2d.placeOn plane)
          (VectorBounds2d.placeOn plane)
          vectorCurve2d.compiled
  new compiledPlanar (on plane vectorCurve2d.derivative)

line :: Vector3d space units -> Vector3d space units -> VectorCurve3d space units
line v1 v2 = bezier (NonEmpty.two v1 v2)

arc ::
  Vector3d space units ->
  Vector3d space units ->
  Angle ->
  Angle ->
  VectorCurve3d space units
arc v1 v2 a b
  | v1 == Vector3d.zero && v2 == Vector3d.zero = zero
  | a == b = constant (Angle.cos a .*. v1 .+. Angle.sin a .*. v2)
  | otherwise = do
      let angle = Curve.line a b
      v1 .*. Curve.cos angle .+. v2 .*. Curve.sin angle

quadraticBezier ::
  Vector3d space units ->
  Vector3d space units ->
  Vector3d space units ->
  VectorCurve3d space units
quadraticBezier v1 v2 v3 = bezier (NonEmpty.three v1 v2 v3)

cubicBezier ::
  Vector3d space units ->
  Vector3d space units ->
  Vector3d space units ->
  Vector3d space units ->
  VectorCurve3d space units
cubicBezier v1 v2 v3 v4 = bezier (NonEmpty.four v1 v2 v3 v4)

bezier :: NonEmpty (Vector3d space units) -> VectorCurve3d space units
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezier (Bezier.derivative controlPoints))

startValue :: VectorCurve3d space units -> Vector3d space units
startValue curve = evaluate curve 0

endValue :: VectorCurve3d space units -> Vector3d space units
endValue curve = evaluate curve 1

desingularize ::
  Maybe (Vector3d space units, Vector3d space units) ->
  VectorCurve3d space units ->
  Maybe (Vector3d space units, Vector3d space units) ->
  VectorCurve3d space units
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
  VectorCurve3d space units ->
  VectorCurve3d space units ->
  VectorCurve3d space units ->
  VectorCurve3d space units
desingularized start middle end =
  new
    (CompiledFunction.desingularized Curve.t.compiled start.compiled middle.compiled end.compiled)
    (desingularized start.derivative middle.derivative end.derivative)

evaluate :: VectorCurve3d space units -> Number -> Vector3d space units
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> VectorCurve3d space units -> Vector3d space units
evaluateAt tValue curve = evaluate curve tValue

evaluateBounds :: VectorCurve3d space units -> Bounds Unitless -> VectorBounds3d space units
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

reverse :: VectorCurve3d space units -> VectorCurve3d space units
reverse curve = curve `compose` (1 -. Curve.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d space units3)
quotient lhs rhs = Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve3d space (units1 ?/? units2))
quotient_ numerator denominator =
  if denominator ~= Curve.zero
    then Error DivisionByZero
    else Ok do
      let singularity0 =
            if Curve.evaluate denominator 0 ~= Quantity.zero
              then Just (lhopital numerator denominator 0)
              else Nothing
      let singularity1 =
            if Curve.evaluate denominator 1 ~= Quantity.zero
              then Just (lhopital numerator denominator 1)
              else Nothing
      desingularize singularity0 (unsafeQuotient_ numerator denominator) singularity1

lhopital ::
  Tolerance units2 =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  Number ->
  (Vector3d space (units1 ?/? units2), Vector3d space (units1 ?/? units2))
lhopital numerator denominator tValue = do
  let numerator' = evaluate numerator.derivative tValue
  let numerator'' = evaluate numerator.derivative.derivative tValue
  let denominator' = Curve.evaluate denominator.derivative tValue
  let denominator'' = Curve.evaluate denominator.derivative.derivative tValue
  let value = numerator' ?/? denominator'
  let firstDerivative =
        Units.simplify $
          (numerator'' ?*? denominator' .-. numerator' ?*? denominator'')
            ?/? (2 *. Quantity.squared_ denominator')
  (value, firstDerivative)

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorCurve3d space units1 ->
  Curve units2 ->
  VectorCurve3d space units3
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient_ numerator denominator)

unsafeQuotient_ ::
  VectorCurve3d space units1 ->
  Curve units2 ->
  VectorCurve3d space (units1 ?/? units2)
unsafeQuotient_ numerator denominator = do
  let quotientDerivative = Units.simplify do
        unsafeQuotient_
          (numerator.derivative ?*? denominator .-. numerator ?*? denominator.derivative)
          (Curve.squared_ denominator)
  new (numerator.compiled ?/? denominator.compiled) quotientDerivative

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d space units1 -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude_ curve)

squaredMagnitude_ :: VectorCurve3d space units -> Curve (units ?*? units)
squaredMagnitude_ curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3d.squaredMagnitude_
          Vector3d.squaredMagnitude_
          VectorBounds3d.squaredMagnitude_
          curve.compiled
  let squaredMagnitudeDerivative = 2 *. curve `dot_` curve.derivative
  Curve.new compiledSquaredMagnitude squaredMagnitudeDerivative

data HasZero = HasZero deriving (Eq, Show)

magnitude :: Tolerance units => VectorCurve3d space units -> Curve units
magnitude curve = Curve.sqrt_ (squaredMagnitude_ curve)

data IsZero = IsZero deriving (Eq, Show)

zeros :: Tolerance units => VectorCurve3d space units -> Result IsZero (List Number)
zeros curve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve.zeros (squaredMagnitude_ curve)) of
    Ok zeros1d -> Ok (List.map (.location) zeros1d)
    Error Curve.IsZero -> Error IsZero

direction ::
  Tolerance units =>
  VectorCurve3d space units ->
  Result IsZero (DirectionCurve3d space)
direction curve = case quotient curve (magnitude curve) of
  Error DivisionByZero -> Error IsZero
  Ok normalizedCurve -> Ok (DirectionCurve3d.unsafe normalizedCurve)

placeIn ::
  Frame3d global frameUnits (Defines local) ->
  VectorCurve3d local units ->
  VectorCurve3d global units
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve3d.placeIn frame)
          (Vector3d.placeIn frame)
          (VectorBounds3d.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame3d global frameUnits (Defines local) ->
  VectorCurve3d global units ->
  VectorCurve3d local units
relativeTo frame = placeIn (Frame3d.inverse frame)
