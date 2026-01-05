module OpenSolid.VectorCurve2d
  ( VectorCurve2d
  , Compiled
  , new
  , recursive
  , compiled
  , derivative
  , startValue
  , endValue
  , evaluate
  , evaluateAt
  , evaluateBounds
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
  , unsafeQuotient
  , unsafeQuotient_
  , magnitude
  , squaredMagnitude
  , squaredMagnitude_
  , reverse
  , isZero
  , IsZero (IsZero)
  , zeros
  , HasZero (HasZero)
  , direction
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  , rotateBy
  , convert
  , unconvert
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
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Direction2d (Direction2d)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Polymorphic.Point2d (Point2d)
import OpenSolid.Polymorphic.Point2d qualified as Point2d
import OpenSolid.Polymorphic.Vector2d (Vector2d (Vector2d))
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

data VectorCurve2d units space
  = VectorCurve2d
  { compiled :: Compiled units space
  , derivative :: ~(VectorCurve2d units space)
  }

type Compiled units space =
  CompiledFunction
    Number
    (Vector2d units space)
    (Bounds Unitless)
    (VectorBounds2d units space)

instance HasField "xComponent" (VectorCurve2d units space) (Curve units) where
  getField = xComponent

instance HasField "yComponent" (VectorCurve2d units space) (Curve units) where
  getField = yComponent

instance HasField "components" (VectorCurve2d units space) (Curve units, Curve units) where
  getField = components

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve2d units1 space) (Curve units2)
  where
  getField = squaredMagnitude

instance
  HasField
    "squaredMagnitude_"
    (VectorCurve2d units space)
    (Curve (units ?*? units))
  where
  getField = squaredMagnitude_

instance FFI (VectorCurve2d Unitless FFI.Space) where
  representation = FFI.classRepresentation "VectorCurve2d"

instance FFI (VectorCurve2d Meters FFI.Space) where
  representation = FFI.classRepresentation "DisplacementCurve2d"

instance FFI (VectorCurve2d Unitless UvSpace) where
  representation = FFI.classRepresentation "UvVectorCurve"

instance HasUnits (VectorCurve2d units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d units1 space1) (VectorCurve2d units2 space2)
  where
  coerce curve = VectorCurve2d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance ApproximateEquality (VectorCurve2d units space) units where
  curve1 ~= curve2 = do
    let equalValues t = evaluate curve1 t ~= evaluate curve2 t
    NonEmpty.allSatisfy equalValues Parameter.samples

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve2d units1 space1)
    (Vector2d units2 space2)
    units1
  where
  curve `intersects` vector = Tolerance.using (Quantity.squared_ ?tolerance) do
    (curve .-. vector).squaredMagnitude_ `intersects` Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector2d units1 space1)
    (VectorCurve2d units2 space2)
    units1
  where
  vector `intersects` curve = curve `intersects` vector

instance Negation (VectorCurve2d units space) where
  negative curve = new (negative curve.compiled) (negative curve.derivative)

instance Multiplication Sign (VectorCurve2d units space) (VectorCurve2d units space) where
  Positive .*. curve = curve
  Negative .*. curve = negative curve

instance Multiplication (VectorCurve2d units space) Sign (VectorCurve2d units space) where
  curve .*. Positive = curve
  curve .*. Negative = negative curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (lhs.derivative .+. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2d units1 space1)
    (Vector2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  curve .+. vector = curve .+. constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d units1 space1)
    (VectorCurve2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  vector .+. curve = constant vector .+. curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2d units1 space1)
    (Vector2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  curve .-. vector = curve .-. constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d units1 space1)
    (VectorCurve2d units2 space2)
    (VectorCurve2d units1 space1)
  where
  vector .-. curve = constant vector .-. curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve units1)
    (VectorCurve2d units2 space)
    (VectorCurve2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve units1)
    (VectorCurve2d units2 space)
    (VectorCurve2d (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new (lhs.compiled ?*? rhs.compiled) (lhs.derivative ?*? rhs .+. lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (VectorCurve2d units2 space) (VectorCurve2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorCurve2d units2 space)
    (VectorCurve2d (units1 ?*? units2) space)
  where
  c1 ?*? c2 = Curve.constant c1 ?*? c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d units1 space) (Curve units2) (VectorCurve2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve2d units1 space)
    (Curve units2)
    (VectorCurve2d (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new (lhs.compiled ?*? rhs.compiled) (lhs.derivative ?*? rhs .+. lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d units1 space) (Quantity units2) (VectorCurve2d units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d units1 space) (Quantity units2) (VectorCurve2d units3 space)
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve2d units1 space)
    (Quantity units2)
    (VectorCurve2d (units1 ?/? units2) space)
  where
  curve ?/? value = Units.simplify (curve ?*? (1 /? value))

instance
  Multiplication_
    (VectorCurve2d units1 space)
    (Quantity units2)
    (VectorCurve2d (units1 ?*? units2) space)
  where
  curve ?*? value = curve ?*? Curve.constant value

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve.new
      (lhs.compiled `dot_` rhs.compiled)
      (lhs.derivative `dot_` rhs .+. lhs `dot_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve2d units1 space1) (Vector2d units2 space2) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve2d units1 space1)
    (Vector2d units2 space2)
    (Curve (units1 ?*? units2))
  where
  curve `dot_` vector = curve `dot_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d units1 space1) (VectorCurve2d units2 space2) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve (units1 ?*? units2))
  where
  vector `dot_` curve = constant vector `dot_` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2d units space1) (Direction2d space2) (Curve units)
  where
  lhs `dot` rhs = lhs `dot` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (VectorCurve2d units space2) (Curve units)
  where
  lhs `dot` rhs = Vector2d.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve (units1 ?*? units2))
  where
  lhs `cross_` rhs =
    Curve.new
      (lhs.compiled `cross_` rhs.compiled)
      (lhs.derivative `cross_` rhs .+. lhs `cross_` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d units1 space1)
    (Vector2d units2 space2)
    (Curve units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve2d units1 space1)
    (Vector2d units2 space2)
    (Curve (units1 ?*? units2))
  where
  curve `cross_` vector = curve `cross_` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve (units1 ?*? units2))
  where
  vector `cross_` curve = constant vector `cross_` curve

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2d units space1) (Direction2d space2) (Curve units)
  where
  lhs `cross` rhs = lhs `cross` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (VectorCurve2d units space2) (Curve units)
  where
  lhs `cross` rhs = Vector2d.unit lhs `cross` rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve2d units1 space1)
  where
  point .+. curve = Curve2d.constant point .+. curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve2d units1 space1)
  where
  point .-. curve = Curve2d.constant point .-. curve

instance
  Composition
    (Curve Unitless)
    (VectorCurve2d units space)
    (VectorCurve2d units space)
  where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve2d units space)
    (VectorSurfaceFunction2d units space)
  where
  curve `compose` function =
    VectorSurfaceFunction2d.new
      (curve.compiled `compose` function.compiled)
      (\p -> (curve.derivative `compose` function) .*. SurfaceFunction.derivative p function)

instance
  Composition
    SurfaceParameter
    (VectorCurve2d units space)
    (VectorSurfaceFunction2d units space)
  where
  curve `compose` parameter = curve `compose` SurfaceFunction.parameter parameter

compiled :: VectorCurve2d units space -> Compiled units space
compiled (VectorCurve2d c _) = c

derivative :: VectorCurve2d units space -> VectorCurve2d units space
derivative (VectorCurve2d _ d) = d

transformBy ::
  Transform2d tag translationUnits space ->
  VectorCurve2d units space ->
  VectorCurve2d units space
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.VectorCurve2d.transformBy transform)
          (Vector2d.transformBy transform)
          (VectorBounds2d.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

rotateBy ::
  forall units space.
  Angle ->
  VectorCurve2d units space ->
  VectorCurve2d units space
rotateBy angle = transformBy (Transform2d.rotateAround (Point2d.origin @units @space) angle)

new :: Compiled units space -> VectorCurve2d units space -> VectorCurve2d units space
new = VectorCurve2d

recursive ::
  Compiled units space ->
  (VectorCurve2d units space -> VectorCurve2d units space) ->
  VectorCurve2d units space
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

-- | The constant zero vector.
zero :: VectorCurve2d units space
zero = constant Vector2d.zero

-- | Create a curve with a constant value.
constant :: Vector2d units space -> VectorCurve2d units space
constant value = new (CompiledFunction.constant value) zero

unit :: DirectionCurve2d space -> VectorCurve2d Unitless space
unit = DirectionCurve2d.unwrap

-- | Create a curve from its X and Y component curves.
xy :: forall units space. Curve units -> Curve units -> VectorCurve2d units space
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Vector2d
          VectorBounds2d
          x.compiled
          y.compiled
  new compiledXY (xy x.derivative y.derivative)

interpolateFrom :: Vector2d units space -> Vector2d units space -> VectorCurve2d units space
interpolateFrom v1 v2 = bezier (NonEmpty.two v1 v2)

arc ::
  Vector2d units space ->
  Vector2d units space ->
  Angle ->
  Angle ->
  VectorCurve2d units space
arc v1 v2 a b
  | v1 == Vector2d.zero && v2 == Vector2d.zero = zero
  | a == b = constant (Angle.cos a .*. v1 .+. Angle.sin a .*. v2)
  | otherwise = do
      let angle = Curve.interpolateFrom a b
      v1 .*. Curve.cos angle .+. v2 .*. Curve.sin angle

quadraticBezier ::
  Vector2d units space ->
  Vector2d units space ->
  Vector2d units space ->
  VectorCurve2d units space
quadraticBezier v1 v2 v3 = bezier (NonEmpty.three v1 v2 v3)

cubicBezier ::
  Vector2d units space ->
  Vector2d units space ->
  Vector2d units space ->
  Vector2d units space ->
  VectorCurve2d units space
cubicBezier v1 v2 v3 v4 = bezier (NonEmpty.four v1 v2 v3 v4)

bezier :: NonEmpty (Vector2d units space) -> VectorCurve2d units space
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezier (Bezier.derivative controlPoints))

startValue :: VectorCurve2d units space -> Vector2d units space
startValue curve = evaluate curve 0

endValue :: VectorCurve2d units space -> Vector2d units space
endValue curve = evaluate curve 1

desingularize ::
  Maybe (Vector2d units space, Vector2d units space) ->
  VectorCurve2d units space ->
  Maybe (Vector2d units space, Vector2d units space) ->
  VectorCurve2d units space
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
  VectorCurve2d units space ->
  VectorCurve2d units space ->
  VectorCurve2d units space ->
  VectorCurve2d units space
desingularized start middle end =
  new
    (CompiledFunction.desingularized Curve.t.compiled start.compiled middle.compiled end.compiled)
    (desingularized start.derivative middle.derivative end.derivative)

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: VectorCurve2d units space -> Number -> Vector2d units space
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> VectorCurve2d units space -> Vector2d units space
evaluateAt tValue curve = evaluate curve tValue

evaluateBounds :: VectorCurve2d units space -> Bounds Unitless -> VectorBounds2d units space
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

-- | Get the X coordinate of a 2D curve as a scalar curve.
xComponent :: VectorCurve2d units space -> Curve units
xComponent curve = do
  let compiledXComponent =
        CompiledFunction.map
          Expression.xComponent
          Vector2d.xComponent
          VectorBounds2d.xComponent
          curve.compiled
  Curve.new compiledXComponent (xComponent curve.derivative)

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yComponent :: VectorCurve2d units space -> Curve units
yComponent curve = do
  let compiledYComponent =
        CompiledFunction.map
          Expression.yComponent
          Vector2d.yComponent
          VectorBounds2d.yComponent
          curve.compiled
  Curve.new compiledYComponent (yComponent curve.derivative)

components :: VectorCurve2d units space -> (Curve units, Curve units)
components curve = (xComponent curve, yComponent curve)

reverse :: VectorCurve2d units space -> VectorCurve2d units space
reverse curve = curve `compose` (1 -. Curve.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2d units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve2d units3 space)
quotient lhs rhs = Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  VectorCurve2d units1 space ->
  Curve units2 ->
  Result DivisionByZero (VectorCurve2d (units1 ?/? units2) space)
quotient_ numerator denominator =
  if denominator ~= Curve.zero
    then Error DivisionByZero
    else Ok do
      let singularity0 =
            if Curve.evaluate denominator 0 ~= Quantity.zero
              then Just (lHopital numerator denominator 0)
              else Nothing
      let singularity1 =
            if Curve.evaluate denominator 1 ~= Quantity.zero
              then Just (lHopital numerator denominator 1)
              else Nothing
      desingularize singularity0 (unsafeQuotient_ numerator denominator) singularity1

lHopital ::
  VectorCurve2d units1 space ->
  Curve units2 ->
  Number ->
  (Vector2d (units1 ?/? units2) space, Vector2d (units1 ?/? units2) space)
lHopital numerator denominator tValue = do
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
  VectorCurve2d units1 space ->
  Curve units2 ->
  VectorCurve2d units3 space
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient_ numerator denominator)

unsafeQuotient_ ::
  VectorCurve2d units1 space ->
  Curve units2 ->
  VectorCurve2d (units1 ?/? units2) space
unsafeQuotient_ numerator denominator = do
  let compiledQuotient = numerator.compiled ?/? denominator.compiled
  let quotientDerivative = Units.simplify do
        unsafeQuotient_
          (derivative numerator ?*? denominator .-. numerator ?*? Curve.derivative denominator)
          (Curve.squared_ denominator)
  new compiledQuotient quotientDerivative

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d units1 space -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude_ curve)

squaredMagnitude_ :: VectorCurve2d units space -> Curve (units ?*? units)
squaredMagnitude_ curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.VectorCurve2d.squaredMagnitude_
          Vector2d.squaredMagnitude_
          VectorBounds2d.squaredMagnitude_
          curve.compiled
  let squaredMagnitudeDerivative = 2 *. curve `dot_` derivative curve
  Curve.new compiledSquaredMagnitude squaredMagnitudeDerivative

data HasZero = HasZero deriving (Eq, Show)

magnitude :: Tolerance units => VectorCurve2d units space -> Curve units
magnitude curve = Curve.sqrt_ (squaredMagnitude_ curve)

sampleValues :: VectorCurve2d units space -> NonEmpty (Vector2d units space)
sampleValues curve = NonEmpty.map (evaluate curve) Parameter.samples

isZero :: Tolerance units => VectorCurve2d units space -> Bool
isZero curve = NonEmpty.allSatisfy (~= Vector2d.zero) (sampleValues curve)

data IsZero = IsZero deriving (Eq, Show)

zeros :: Tolerance units => VectorCurve2d units space -> Result IsZero (List Number)
zeros curve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve.zeros curve.squaredMagnitude_) of
    Ok zeros1d -> Ok (List.map (.location) zeros1d)
    Error Curve.IsZero -> Error IsZero

direction ::
  Tolerance units =>
  VectorCurve2d units space ->
  Result IsZero (DirectionCurve2d space)
direction curve = case quotient curve (magnitude curve) of
  Error DivisionByZero -> Error IsZero
  Ok normalizedCurve -> Ok (DirectionCurve2d.unsafe normalizedCurve)

placeIn ::
  Frame2d frameUnits global local ->
  VectorCurve2d units local ->
  VectorCurve2d units global
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve2d.placeIn frame)
          (Vector2d.placeIn frame)
          (VectorBounds2d.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame2d frameUnits global local ->
  VectorCurve2d units global ->
  VectorCurve2d units local
relativeTo frame = placeIn (Frame2d.inverse frame)

placeOn ::
  Plane3d global local ->
  VectorCurve2d units local ->
  VectorCurve3d units global
placeOn plane curve = VectorCurve3d.on plane curve

convert ::
  Quantity (units2 ?/? units1) ->
  VectorCurve2d units1 space ->
  VectorCurve2d units2 space
convert factor curve = Units.simplify (curve ?*? factor)

unconvert ::
  Quantity (units2 ?/? units1) ->
  VectorCurve2d units2 space ->
  VectorCurve2d units1 space
unconvert factor curve = Units.simplify (curve ?/? factor)
