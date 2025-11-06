module OpenSolid.Curve
  ( Curve
  , Compiled
  , Zero
  , isEndpoint
  , evaluate
  , evaluateAt
  , evaluateBounds
  , startValue
  , endValue
  , derivative
  , new
  , concrete
  , recursive
  , zero
  , constant
  , desingularize
  , desingularized
  , t
  , line
  , bezier
  , hermite
  , quadraticSpline
  , cubicSpline
  , rationalBezier
  , rationalQuadraticSpline
  , rationalCubicSpline
  , quotient
  , quotient#
  , unsafeQuotient
  , unsafeQuotient#
  , squared
  , squared#
  , sqrt
  , sqrt#
  , unsafeSqrt
  , unsafeSqrt#
  , cubed
  , sin
  , cos
  , IsZero (IsZero)
  , zeros
  , CrossesZero (CrossesZero)
  , sign
  , reverse
  , integrate
  , b00
  , b01
  , b02
  , b10
  , b11
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve.Zero (Zero)
import OpenSolid.Curve.Zero qualified as Zero
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Domain1d (Domain1d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Error qualified as Error
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Exception qualified as Exception
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Solve1d qualified as Solve1d
import OpenSolid.Stream (Stream)
import OpenSolid.Stream qualified as Stream
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d qualified as VectorCurve2d
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import Prelude ((+), (/))

data Curve units where
  Curve :: Compiled units -> ~(Curve units) -> Curve units

type Compiled units = CompiledFunction Number (Quantity units) (Bounds Unitless) (Bounds units)

instance HasField "compiled" (Curve units) (Compiled units) where
  getField (Curve c _) = c

instance HasField "derivative" (Curve units) (Curve units) where
  getField = derivative

instance FFI (Curve Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve Meters) where
  representation = FFI.classRepresentation "LengthCurve"

instance FFI (Curve SquareMeters) where
  representation = FFI.classRepresentation "AreaCurve"

instance FFI (Curve Radians) where
  representation = FFI.classRepresentation "AngleCurve"

instance HasUnits (Curve units) units

instance Units.Coercion (Curve units1) (Curve units2) where
  coerce curve = Curve (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance ApproximateEquality (Curve units) units where
  curve1 ~= curve2 =
    List.allTrue [evaluate curve1 tValue ~= evaluate curve2 tValue | tValue <- Parameter.samples]

instance
  units1 ~ units2 =>
  Intersects (Curve units1) (Quantity units2) units1
  where
  curve `intersects` value =
    -- TODO optimize this to use a special Solve1d.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and their exact locations)
    case zeros (curve .-. value) of
      Success [] -> False
      Success List.OneOrMore -> True
      Failure IsZero -> True

instance
  units1 ~ units2 =>
  Intersects (Quantity units1) (Curve units2) units1
  where
  value `intersects` curve = curve `intersects` value

isEndpoint :: Number -> Bool
isEndpoint tValue = tValue == 0 || tValue == 1

new :: Compiled units -> Curve units -> Curve units
new = Curve

concrete :: Expression Number (Quantity units) -> Curve units -> Curve units
concrete givenExpression givenDerivative =
  new (CompiledFunction.concrete givenExpression) givenDerivative

recursive :: Compiled units -> (Curve units -> Curve units) -> Curve units
recursive givenCompiled derivativeFunction =
  let result = Curve givenCompiled (derivativeFunction result) in result

-- | A curve equal to zero everywhere.
zero :: Curve units
zero = constant Quantity.zero

-- | Create a curve with the given constant value.
constant :: Quantity units -> Curve units
constant value = new (CompiledFunction.constant value) zero

{-| A curve parameter.

In other words, a curve whose value is equal to its input parameter.
When defining parametric curves, you will typically start with 'Curve.t'
and then use arithmetic operators etc. to build up more complex curves.
-}
t :: Curve Unitless
t = new (CompiledFunction.concrete Expression.t) (constant 1)

-- | Create a curve that linearly interpolates from the first value to the second.
line :: Quantity units -> Quantity units -> Curve units
line a b = a .+. t .*. (b .-. a)

-- | Get the derivative of a curve.
derivative :: Curve units -> Curve units
derivative (Curve _ d) = d

instance Negation (Curve units) where
  negative curve = new (negative curve.compiled) (negative curve.derivative)

instance Multiplication Sign (Curve units) (Curve units) where
  Positive .*. curve = curve
  Negative .*. curve = negative curve

instance Multiplication (Curve units) Sign (Curve units) where
  curve .*. Positive = curve
  curve .*. Negative = negative curve

instance units1 ~ units2 => Addition (Curve units1) (Curve units2) (Curve units1) where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (lhs.derivative .+. rhs.derivative)

instance units1 ~ units2 => Addition (Curve units1) (Quantity units2) (Curve units1) where
  curve .+. value = curve .+. constant value

instance units1 ~ units2 => Addition (Quantity units1) (Curve units2) (Curve units1) where
  value .+. curve = constant value .+. curve

instance units1 ~ units2 => Subtraction (Curve units1) (Curve units2) (Curve units1) where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (lhs.derivative .-. rhs.derivative)

instance
  units1 ~ units2 =>
  Subtraction (Curve units1) (Quantity units2) (Curve units1)
  where
  curve .-. value = curve .-. constant value

instance
  units1 ~ units2 =>
  Subtraction (Quantity units1) (Curve units2) (Curve units1)
  where
  value .-. curve = constant value .-. curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Curve units2) (Curve units3)
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance Multiplication# (Curve units1) (Curve units2) (Curve (units1 #*# units2)) where
  lhs #*# rhs =
    new (lhs.compiled #*# rhs.compiled) (lhs.derivative #*# rhs .+. lhs #*# rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Quantity units2) (Curve units3)
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance Multiplication# (Curve units1) (Quantity units2) (Curve (units1 #*# units2)) where
  curve #*# value = curve #*# constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Curve units2) (Curve units3)
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance Multiplication# (Quantity units1) (Curve units2) (Curve (units1 #*# units2)) where
  value #*# curve = constant value #*# curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Vector2d (space @ units2)) (VectorCurve2d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Curve units1)
    (Vector2d (space @ units2))
    (VectorCurve2d (space @ (units1 #*# units2)))
  where
  curve #*# vector = curve #*# VectorCurve2d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Curve units2) (VectorCurve2d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Vector2d (space @ units1))
    (Curve units2)
    (VectorCurve2d (space @ (units1 #*# units2)))
  where
  vector #*# curve = VectorCurve2d.constant vector #*# curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Vector3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Curve units1)
    (Vector3d (space @ units2))
    (VectorCurve3d (space @ (units1 #*# units2)))
  where
  curve #*# vector = curve #*# VectorCurve3d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Vector3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 #*# units2)))
  where
  vector #*# curve = VectorCurve3d.constant vector #*# curve

instance Composition (Curve Unitless) (Curve units) (Curve units) where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) .*. g.derivative)

reverse :: Curve units -> Curve units
reverse curve = curve `compose` (1 -. t)

bezier :: NonEmpty (Quantity units) -> Curve units
bezier controlPoints =
  new
    @ CompiledFunction.concrete (Expression.bezierCurve controlPoints)
    @ bezier (Bezier.derivative controlPoints)

hermite ::
  Quantity units ->
  List (Quantity units) ->
  Quantity units ->
  List (Quantity units) ->
  Curve units
hermite value0 derivatives0 value1 derivatives1 =
  bezier (Bezier.hermite value0 derivatives0 value1 derivatives1)

quadraticSpline :: Quantity units -> Quantity units -> Quantity units -> Curve units
quadraticSpline p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

cubicSpline :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Curve units
cubicSpline p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

rationalBezier ::
  Tolerance Unitless =>
  NonEmpty (Quantity units, Number) ->
  Result DivisionByZero (Curve units)
rationalBezier pointsAndWeights = do
  let scaledPoint (point, weight) = point .*. weight
  quotient
    @ bezier (NonEmpty.map scaledPoint pointsAndWeights)
    @ bezier (NonEmpty.map Pair.second pointsAndWeights)

rationalQuadraticSpline ::
  Tolerance Unitless =>
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  Result DivisionByZero (Curve units)
rationalQuadraticSpline pw1 pw2 pw3 = rationalBezier (NonEmpty.three pw1 pw2 pw3)

rationalCubicSpline ::
  Tolerance Unitless =>
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  Result DivisionByZero (Curve units)
rationalCubicSpline pw1 pw2 pw3 pw4 = rationalBezier (NonEmpty.four pw1 pw2 pw3 pw4)

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
{-# INLINE evaluate #-}
evaluate :: Curve units -> Number -> Quantity units
evaluate curve = CompiledFunction.evaluate curve.compiled

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> Curve units -> Quantity units
evaluateAt tValue curve = evaluate curve tValue

{-# INLINE evaluateBounds #-}
evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
evaluateBounds curve = CompiledFunction.evaluateBounds curve.compiled

startValue :: Curve units -> Quantity units
startValue curve = evaluate curve 0

endValue :: Curve units -> Quantity units
endValue curve = evaluate curve 1

desingularize ::
  Maybe (Quantity units, Quantity units) ->
  Curve units ->
  Maybe (Quantity units, Quantity units) ->
  Curve units
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

desingularized :: Curve units -> Curve units -> Curve units -> Curve units
desingularized start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (desingularized start.derivative middle.derivative end.derivative)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  Curve units1 ->
  Curve units2 ->
  Result DivisionByZero (Curve units3)
quotient lhs rhs = Units.specialize (quotient# lhs rhs)

quotient# ::
  Tolerance units2 =>
  Curve units1 ->
  Curve units2 ->
  Result DivisionByZero (Curve (units1 #/# units2))
quotient# numerator denominator =
  if denominator ~= zero
    then Failure DivisionByZero
    else Success do
      let singularity0 =
            if evaluate denominator 0 ~= Quantity.zero
              then Just (lhopital numerator denominator 0)
              else Nothing
      let singularity1 =
            if evaluate denominator 1 ~= Quantity.zero
              then Just (lhopital numerator denominator 1)
              else Nothing
      desingularize singularity0 (unsafeQuotient# numerator denominator) singularity1

lhopital ::
  Tolerance units2 =>
  Curve units1 ->
  Curve units2 ->
  Number ->
  (Quantity (units1 #/# units2), Quantity (units1 #/# units2))
lhopital numerator denominator tValue = do
  let numerator' = evaluate numerator.derivative tValue
  let numerator'' = evaluate numerator.derivative.derivative tValue
  let denominator' = evaluate denominator.derivative tValue
  let denominator'' = evaluate denominator.derivative.derivative tValue
  let value = numerator' #/# denominator'
  let firstDerivative =
        Units.simplify $
          (numerator'' #*# denominator' .-. numerator' #*# denominator'')
            #/# (2 *. Quantity.squared# denominator')
  (value, firstDerivative)

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  Curve units1 ->
  Curve units2 ->
  Curve units3
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient# numerator denominator)

unsafeQuotient# :: Curve units1 -> Curve units2 -> Curve (units1 #/# units2)
unsafeQuotient# numerator denominator = do
  let quotientCompiled = numerator.compiled #/# denominator.compiled
  let quotientDerivative = Units.simplify do
        unsafeQuotient#
          (numerator.derivative #*# denominator .-. numerator #*# denominator.derivative)
          (squared# denominator)
  new quotientCompiled quotientDerivative

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve units1) (Quantity units2) (Curve units3)
  where
  lhs ./. rhs = Units.specialize (lhs #/# rhs)

instance Division# (Curve units1) (Quantity units2) (Curve (units1 #/# units2)) where
  curve #/# value = Units.simplify (curve #*# (1 /# value))

-- | Compute the square of a curve.
squared :: Units.Squared units1 units2 => Curve units1 -> Curve units2
squared curve = Units.specialize (squared# curve)

squared# :: Curve units -> Curve (units #*# units)
squared# curve =
  new
    (CompiledFunction.map Expression.squared# Quantity.squared# Bounds.squared# curve.compiled)
    (2 *. curve #*# curve.derivative)

-- | Compute the square root of a curve.
sqrt :: Tolerance units1 => Units.Squared units1 units2 => Curve units2 -> Curve units1
sqrt curve = sqrt# (Units.unspecialize curve)

sqrt# :: Tolerance units => Curve (units #*# units) -> Curve units
sqrt# curve
  | Tolerance.using Tolerance.squared# (curve ~= zero) = zero
  | otherwise = do
      let firstDerivative = curve.derivative
      let secondDerivative = firstDerivative.derivative
      let isSingularity tValue = do
            let curveIsZero =
                  Tolerance.using Tolerance.squared# $
                    evaluate curve tValue ~= Quantity.zero
            let secondDerivativeValue = evaluate secondDerivative tValue
            let firstDerivativeTolerance =
                  ?tolerance #*# Quantity.sqrt# (2 *. secondDerivativeValue)
            let firstDerivativeIsZero =
                  Tolerance.using firstDerivativeTolerance $
                    evaluate firstDerivative tValue ~= Quantity.zero
            curveIsZero && firstDerivativeIsZero
      let singularity0 =
            if isSingularity 0
              then Just (Quantity.zero, Quantity.sqrt# (0.5 *. evaluate secondDerivative 0))
              else Nothing
      let singularity1 =
            if isSingularity 1
              then Just (Quantity.zero, negative (Quantity.sqrt# (0.5 *. evaluate secondDerivative 1)))
              else Nothing
      desingularize singularity0 (unsafeSqrt# curve) singularity1

unsafeSqrt :: Units.Squared units1 units2 => Curve units2 -> Curve units1
unsafeSqrt curve = unsafeSqrt# (Units.unspecialize curve)

unsafeSqrt# :: Curve (units #*# units) -> Curve units
unsafeSqrt# curve =
  recursive
    @ CompiledFunction.map Expression.sqrt# Quantity.sqrt# Bounds.sqrt# curve.compiled
    @ \self -> Units.coerce (unsafeQuotient# curve.derivative (2 *. self))

-- | Compute the cube of a curve.
cubed :: Curve Unitless -> Curve Unitless
cubed curve =
  new
    (CompiledFunction.map Expression.cubed Number.cubed Bounds.cubed curve.compiled)
    (3 *. squared curve .*. curve.derivative)

-- | Compute the sine of a curve.
sin :: Curve Radians -> Curve Unitless
sin curve =
  new
    (CompiledFunction.map Expression.sin Angle.sin Bounds.sin curve.compiled)
    (cos curve .*. (curve.derivative ./. Angle.radian))

-- | Compute the cosine of a curve.
cos :: Curve Radians -> Curve Unitless
cos curve =
  new
    (CompiledFunction.map Expression.cos Angle.cos Bounds.cos curve.compiled)
    (negative (sin curve) .*. (curve.derivative ./. Angle.radian))

integrate :: Curve units -> Estimate units
integrate curve = Estimate.new (Integral curve curve.derivative Bounds.unitInterval)

data Integral units = Integral (Curve units) (Curve units) (Bounds Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral curve curveDerivative domain) = do
    let dx = Bounds.width domain
    let derivativeBounds = evaluateBounds curveDerivative domain
    let estimate0 = dx .*. evaluateBounds curve domain
    let y1 = evaluate curve (Bounds.lower domain)
    let y2 = evaluate curve (Bounds.upper domain)
    let m = Bounds.width derivativeBounds
    let error1 = 0.125 *. m .*. dx .*. dx
    let estimate1 = dx .*. Quantity.midpoint y1 y2 .+. Bounds (negative error1) error1
    case Bounds.intersection estimate0 estimate1 of
      Just intersection -> intersection
      Nothing -> estimate0 -- Shouldn't happen if bounds are correct

  refineImpl (Integral curve curveDerivative domain) = do
    let (leftDomain, rightDomain) = Bounds.bisect domain
    let leftIntegral = Integral curve curveDerivative leftDomain
    let rightIntegral = Integral curve curveDerivative rightDomain
    Estimate.new leftIntegral .+. Estimate.new rightIntegral

----- ZERO FINDING -----

data IsZero = IsZero deriving (Eq, Show, Error.Message)

{-| Find all points at which the given curve is zero.

This includes not only points where the curve *crosses* zero,
but also where it is *tangent* to zero.
For example, y=x-3 crosses zero at x=3,
while y=(x-3)^2 is tangent to zero at x=3.

We define y=x-3 as having a zero of order 0 at x=3,
since only the "derivative of order zero" (the curve itself)
is zero at that point.
Similarly, y=(x-3)^2 has a zero of order 1 at x=3,
since the first derivative (but not the second derivative)
is zero at that point.

Currently, this function up to third-order zeros
(e.g. y=x^4 has a third-order zero at x=0,
since everything up to the third derivative is zero at x=0).

The current tolerance is used to determine
whether a given point should be considered a zero,
and of what order.
For example, the curve y=x^2-0.0001 is *exactly* zero at x=0.01 and x=-0.01.
However, note that the curve is also very close to zero at x=0,
and at that point the first derivative is *also* zero.
In many cases, it is reasonable to assume that
the 0.0001 is an artifact of numerical roundoff,
and the curve actually has a single zero of order 1 at x=0.
The current tolerance is used to choose which case to report.
In this example, a tolerance of 0.000001
would mean that we consider 0.0001 a meaningful value (not just roundoff),
so we would end up reporting two order-0 zeros at x=0.01 and x=-0.01.
On the other hand, a tolerance of 0.01 would mean that
we consider 0.0001 as just roundoff error,
so we would end up reporting a single order-1 zero at x=0
(the point at which the *first derivative* is zero).
-}
zeros :: Tolerance units => Curve units -> Result IsZero (List Zero)
zeros curve
  | curve ~= zero = Failure IsZero
  | otherwise = do
      let derivatives = Stream.iterate (.derivative) curve
      let derivativeBounds tBounds = Stream.map (\f -> evaluateBounds f tBounds) derivatives
      let cache = Solve1d.init derivativeBounds
      case Solve1d.search (findZeros derivatives) cache of
        Success foundZeros -> Success (List.sortBy (.location) foundZeros)
        Failure Solve1d.InfiniteRecursion -> Exception.higherOrderZero

findZeros ::
  Tolerance units =>
  Stream (Curve units) ->
  Domain1d ->
  Stream (Bounds units) ->
  Solve1d.Exclusions exclusions ->
  Solve1d.Action exclusions Zero
findZeros derivatives subdomain derivativeBounds exclusions
  -- Skip the subdomain entirely if the curve itself is non-zero everywhere
  | not (Stream.head derivativeBounds `intersects` Quantity.zero) = Solve1d.pass
  -- Optimization heuristic: bisect down to small subdomains first,
  -- to quickly eliminate most of the curve based on simple value bounds
  -- before attempting more complex/sophisticated solving
  | Bounds.width (Domain1d.bounds subdomain) > 1 / 1024 = Solve1d.recurse
  | otherwise = case exclusions of
      Solve1d.SomeExclusions -> Solve1d.recurse
      Solve1d.NoExclusions ->
        case findZerosOrder 0 derivatives subdomain derivativeBounds of
          Unresolved -> Solve1d.recurse
          Resolved [] -> Solve1d.pass
          Resolved (NonEmpty subdomainZeros) -> do
            let subdomainInterior = Domain1d.interior subdomain
            let isInterior (t0, _) = Bounds.includes t0 subdomainInterior
            if NonEmpty.allSatisfy isInterior subdomainZeros
              then Solve1d.return (NonEmpty.map toZero subdomainZeros)
              else Solve1d.recurse

toZero :: (Number, Solve1d.Neighborhood units) -> Zero
toZero (t0, neighborhood) = Solve1d.zero t0 neighborhood

maxZeroOrder :: Int
maxZeroOrder = 3

findZerosOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve units) ->
  Domain1d ->
  Stream (Bounds units) ->
  Fuzzy (List (Number, Solve1d.Neighborhood units))
findZerosOrder k derivatives subdomain derivativeBounds
  -- A derivative is resolved, so it has no zeros
  -- (note that if k == 0, we already checked for the curve being non-zero in findZeros above)
  | k > 0 && Bounds.isResolved (Stream.head derivativeBounds) = Resolved []
  -- We've exceeded the maximum zero order without finding a non-zero derivative
  | k > maxZeroOrder = Unresolved
  -- Otherwise, find higher-order zeros and then search in between them
  | otherwise = do
      let higherDerivatives = Stream.tail derivatives
      let higherDerivativeBounds = Stream.tail derivativeBounds
      let currentDerivative = Stream.head derivatives
      let nextDerivative = Stream.head higherDerivatives
      let tBounds = Domain1d.bounds subdomain
      higherOrderZeros <- findZerosOrder (k + 1) higherDerivatives subdomain higherDerivativeBounds
      case higherOrderZeros of
        [] -> solveMonotonic k currentDerivative nextDerivative tBounds
        List.One (t0, neighborhood) -> do
          if Quantity.abs (evaluate currentDerivative t0)
            <= Solve1d.derivativeTolerance neighborhood k
            then Resolved [(t0, neighborhood)]
            else do
              let leftBounds = Bounds (Bounds.lower tBounds) t0
              let rightBounds = Bounds t0 (Bounds.upper tBounds)
              leftZeros <- solveMonotonic k currentDerivative nextDerivative leftBounds
              rightZeros <- solveMonotonic k currentDerivative nextDerivative rightBounds
              Resolved (leftZeros <> rightZeros)
        List.TwoOrMore -> Unresolved

solveMonotonic ::
  Tolerance units =>
  Int ->
  Curve units ->
  Curve units ->
  Bounds Unitless ->
  Fuzzy (List (Number, Solve1d.Neighborhood units))
solveMonotonic m fm fn tBounds = do
  let n = m + 1
  let Bounds tLow tHigh = tBounds
  let startNeighborhood = Solve1d.neighborhood n (evaluate fn tLow)
  if Quantity.abs (evaluate fm tLow) <= Solve1d.derivativeTolerance startNeighborhood m
    then if tLow == 0 then Resolved [(0, startNeighborhood)] else Unresolved
    else do
      let endNeighborhood = Solve1d.neighborhood n (evaluate fn tHigh)
      if Quantity.abs (evaluate fm tHigh) <= Solve1d.derivativeTolerance endNeighborhood m
        then if tHigh == 1 then Resolved [(1, endNeighborhood)] else Unresolved
        else do
          case Solve1d.monotonic (evaluate fm) (evaluate fn) tBounds of
            Solve1d.Exact t0 -> Resolved [(t0, Solve1d.neighborhood n (evaluate fn t0))]
            Solve1d.Closest _ -> Unresolved

data CrossesZero = CrossesZero deriving (Eq, Show, Error.Message)

{-| Attempt to find the (consistent) sign of all values on the curve.

Will return an error if the curve crosses zero,
or has an indeterminate higher-order zero anywhere.
If the curve is zero everywhere, then returns positive.
-}
sign :: Tolerance units => Curve units -> Result CrossesZero Sign
sign curve = case zeros curve of
  Failure IsZero -> Success Positive
  Success curveZeros ->
    case List.filter isInnerZero curveZeros of
      [] -> Success (Quantity.sign (evaluate curve 0.5)) -- No inner zeros, so check sign at t=0.5
      NonEmpty innerZeros ->
        case NonEmpty.filter isCrossingZero innerZeros of
          List.OneOrMore -> Failure CrossesZero -- There exists at least one inner crossing zero
          [] -> do
            -- All inner zeros are non-crossing (e.g. quadratic) ones,
            -- so we can safely test the curve
            -- halfway between t=0 and the first inner zero
            let firstInnerZero = NonEmpty.first innerZeros
            let testPoint = 0.5 *. firstInnerZero.location
            Success (Quantity.sign (evaluate curve testPoint))

isInnerZero :: Zero -> Bool
isInnerZero curveZero = not (isEndpoint curveZero.location)

isCrossingZero :: Zero -> Bool
isCrossingZero curveZero =
  -- Curve order 0 is linear (crossing) zero
  -- Curve order 1 is quadratic (non-crossing) zero
  -- Curve order 2 is cubic (crossing) zero
  -- Curve order 3 is quartic (non-crossing) zero, etc.
  Int.isEven curveZero.order

b00 :: Curve Unitless
b00 =
  concrete Expression.b00 $
    concrete Expression.b00d1 $
      concrete Expression.b00d2 $
        concrete Expression.b00d3 $
          constant 72

b01 :: Curve Unitless
b01 =
  concrete Expression.b01 $
    concrete Expression.b01d1 $
      concrete Expression.b01d2 $
        concrete Expression.b01d3 $
          constant 48

b02 :: Curve Unitless
b02 =
  concrete Expression.b02 $
    concrete Expression.b02d1 $
      concrete Expression.b02d2 $
        concrete Expression.b02d3 $
          constant 12

b10 :: Curve Unitless
b10 =
  concrete Expression.b10 $
    concrete Expression.b10d1 $
      concrete Expression.b10d2 $
        concrete Expression.b10d3 $
          constant -72

b11 :: Curve Unitless
b11 =
  concrete Expression.b11 $
    concrete Expression.b11d1 $
      concrete Expression.b11d2 $
        concrete Expression.b11d3 $
          constant 24
