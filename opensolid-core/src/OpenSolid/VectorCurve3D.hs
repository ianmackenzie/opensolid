module OpenSolid.VectorCurve3D
  ( VectorCurve3D
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
  , interpolateFrom
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
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero qualified as Curve1D.Zero
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Direction3D (Direction3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D qualified as DirectionCurve3D
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2D qualified as Expression.VectorCurve2D
import OpenSolid.Expression.VectorCurve3D qualified as Expression.VectorCurve3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
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
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data VectorCurve3D units space
  = VectorCurve3D (Compiled units space) ~(VectorCurve3D units space)

type Compiled units space =
  CompiledFunction
    Number
    (Vector3D units space)
    (Interval Unitless)
    (VectorBounds3D units space)

instance HasField "compiled" (VectorCurve3D units space) (Compiled units space) where
  getField = compiled

instance HasField "derivative" (VectorCurve3D units space) (VectorCurve3D units space) where
  getField = derivative

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve3D units1 space) (Curve1D units2)
  where
  getField = squaredMagnitude

instance
  HasField
    "squaredMagnitude_"
    (VectorCurve3D units space)
    (Curve1D (units ?*? units))
  where
  getField = squaredMagnitude_

instance HasUnits (VectorCurve3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3D units1 space1) (VectorCurve3D units2 space2)
  where
  coerce curve = VectorCurve3D (Units.coerce curve.compiled) (Units.coerce curve.derivative)

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
    (curve .-. vector).squaredMagnitude_ `intersects` Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    units1
  where
  vector `intersects` curve = curve `intersects` vector

instance Negation (VectorCurve3D units space) where
  negative curve = new (negative curve.compiled) (negative curve.derivative)

instance Multiplication Sign (VectorCurve3D units space) (VectorCurve3D units space) where
  Positive .*. curve = curve
  Negative .*. curve = negative curve

instance Multiplication (VectorCurve3D units space) Sign (VectorCurve3D units space) where
  curve .*. Positive = curve
  curve .*. Negative = negative curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (lhs.derivative .+. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  curve .+. vector = curve .+. constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  vector .+. curve = constant vector .+. curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  curve .-. vector = curve .-. constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  vector .-. curve = constant vector .-. curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (lhs.derivative ?*? rhs .+. lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve3D units2 space) (VectorCurve3D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

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
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (lhs.derivative ?*? rhs .+. lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3D units1 space) (Quantity units2) (VectorCurve3D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

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
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D (units1 ?/? units2) space)
  where
  curve ?/? value = Units.simplify (curve ?*? (1 /? value))

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
      (lhs.derivative `dot_` rhs .+. lhs `dot_` rhs.derivative)

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
      (lhs.derivative `cross_` rhs .+. lhs `cross_` rhs.derivative)

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
    (Curve1D Unitless)
    (VectorCurve3D units space)
    (VectorCurve3D units space)
  where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve3D units space)
    (VectorSurfaceFunction3D units space)
  where
  curve `compose` function =
    VectorSurfaceFunction3D.new
      (curve.compiled `compose` function.compiled)
      (\p -> curve.derivative `compose` function .*. SurfaceFunction.derivative p function)

compiled :: VectorCurve3D units space -> Compiled units space
compiled (VectorCurve3D c _) = c

derivative :: VectorCurve3D units space -> VectorCurve3D units space
derivative (VectorCurve3D _ d) = d

transformBy :: Transform3D tag space -> VectorCurve3D units space -> VectorCurve3D units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.VectorCurve3D.transformBy transform)
          (Vector3D.transformBy transform)
          (VectorBounds3D.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

new :: Compiled units space -> VectorCurve3D units space -> VectorCurve3D units space
new = VectorCurve3D

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
          (Expression.VectorCurve2D.placeOn plane)
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
  | a == b = constant (Angle.cos a .*. v1 .+. Angle.sin a .*. v2)
  | otherwise = do
      let angle = Curve1D.interpolateFrom a b
      v1 .*. Curve1D.cos angle .+. v2 .*. Curve1D.sin angle

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
startValue curve = evaluate curve 0

endValue :: VectorCurve3D units space -> Vector3D units space
endValue curve = evaluate curve 1

desingularize ::
  Maybe (Vector3D units space, Vector3D units space) ->
  VectorCurve3D units space ->
  Maybe (Vector3D units space, Vector3D units space) ->
  VectorCurve3D units space
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
  VectorCurve3D units space ->
  VectorCurve3D units space ->
  VectorCurve3D units space ->
  VectorCurve3D units space
desingularized start middle end =
  new
    (CompiledFunction.desingularized Curve1D.t.compiled start.compiled middle.compiled end.compiled)
    (desingularized start.derivative middle.derivative end.derivative)

evaluate :: VectorCurve3D units space -> Number -> Vector3D units space
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> VectorCurve3D units space -> Vector3D units space
evaluateAt tValue curve = evaluate curve tValue

evaluateBounds :: VectorCurve3D units space -> Interval Unitless -> VectorBounds3D units space
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

reverse :: VectorCurve3D units space -> VectorCurve3D units space
reverse curve = curve `compose` (1 -. Curve1D.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve3D units3 space)
quotient lhs rhs = Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve3D (units1 ?/? units2) space)
quotient_ numerator denominator =
  if denominator ~= Curve1D.zero
    then Error DivisionByZero
    else Ok do
      let singularity0 =
            if Curve1D.evaluate denominator 0 ~= Quantity.zero
              then Just (lhopital numerator denominator 0)
              else Nothing
      let singularity1 =
            if Curve1D.evaluate denominator 1 ~= Quantity.zero
              then Just (lhopital numerator denominator 1)
              else Nothing
      desingularize singularity0 (unsafeQuotient_ numerator denominator) singularity1

lhopital ::
  Tolerance units2 =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  Number ->
  (Vector3D (units1 ?/? units2) space, Vector3D (units1 ?/? units2) space)
lhopital numerator denominator tValue = do
  let numerator' = evaluate numerator.derivative tValue
  let numerator'' = evaluate numerator.derivative.derivative tValue
  let denominator' = Curve1D.evaluate denominator.derivative tValue
  let denominator'' = Curve1D.evaluate denominator.derivative.derivative tValue
  let value = numerator' ?/? denominator'
  let firstDerivative =
        Units.simplify $
          (numerator'' ?*? denominator' .-. numerator' ?*? denominator'')
            ?/? (2 *. Quantity.squared_ denominator')
  (value, firstDerivative)

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  VectorCurve3D units3 space
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient_ numerator denominator)

unsafeQuotient_ ::
  VectorCurve3D units1 space ->
  Curve1D units2 ->
  VectorCurve3D (units1 ?/? units2) space
unsafeQuotient_ numerator denominator = do
  let quotientDerivative = Units.simplify do
        unsafeQuotient_
          (numerator.derivative ?*? denominator .-. numerator ?*? denominator.derivative)
          (Curve1D.squared_ denominator)
  new (numerator.compiled ?/? denominator.compiled) quotientDerivative

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3D units1 space -> Curve1D units2
squaredMagnitude curve = Units.specialize (squaredMagnitude_ curve)

squaredMagnitude_ :: VectorCurve3D units space -> Curve1D (units ?*? units)
squaredMagnitude_ curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3D.squaredMagnitude_
          Vector3D.squaredMagnitude_
          VectorBounds3D.squaredMagnitude_
          curve.compiled
  let squaredMagnitudeDerivative = 2 *. curve `dot_` curve.derivative
  Curve1D.new compiledSquaredMagnitude squaredMagnitudeDerivative

data HasZero = HasZero deriving (Eq, Show)

magnitude :: Tolerance units => VectorCurve3D units space -> Curve1D units
magnitude curve = Curve1D.sqrt_ (squaredMagnitude_ curve)

data IsZero = IsZero deriving (Eq, Show)

zeros :: Tolerance units => VectorCurve3D units space -> Result IsZero (List Number)
zeros curve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.zeros (squaredMagnitude_ curve)) of
    Ok zeros1D -> Ok (List.map (.location) zeros1D)
    Error Curve1D.IsZero -> Error IsZero

direction ::
  Tolerance units =>
  VectorCurve3D units space ->
  Result IsZero (DirectionCurve3D space)
direction curve = case quotient curve (magnitude curve) of
  Error DivisionByZero -> Error IsZero
  Ok normalizedCurve -> Ok (DirectionCurve3D.unsafe normalizedCurve)

placeIn ::
  Frame3D global local ->
  VectorCurve3D units local ->
  VectorCurve3D units global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve3D.placeIn frame)
          (Vector3D.placeIn frame)
          (VectorBounds3D.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame3D global local ->
  VectorCurve3D units global ->
  VectorCurve3D units local
relativeTo frame = placeIn (Frame3D.inverse frame)
