module OpenSolid.Curve1D
  ( Curve1D
  , Compiled
  , Zero
  , evaluate
  , evaluateAt
  , evaluateBounds
  , startValue
  , endValue
  , compiled
  , derivative
  , new
  , concrete
  , recursive
  , zero
  , constant
  , desingularize
  , desingularized
  , t
  , interpolateFrom
  , bezier
  , hermite
  , quadraticSpline
  , cubicSpline
  , rationalBezier
  , rationalQuadraticSpline
  , rationalCubicSpline
  , quotient
  , quotient_
  , WithNoZeros (WithNoZeros)
  , WithNoInteriorZeros (WithNoInteriorZeros)
  , squared
  , squared_
  , sqrt
  , sqrt_
  , cubed
  , sin
  , cos
  , singularityTolerance
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
  , newtonRaphson
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.Curve1D.WithNoInteriorZeros qualified as Curve1D.WithNoInteriorZeros
import {-# SOURCE #-} OpenSolid.Curve1D.WithNoZeros qualified as Curve1D.WithNoZeros
import OpenSolid.Curve1D.Zero (Zero)
import OpenSolid.Curve1D.Zero qualified as Zero
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Domain1D (Domain1D)
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
import OpenSolid.Int qualified as Int
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson1D qualified as NewtonRaphson1D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.Solve1D qualified as Solve1D
import OpenSolid.Stream (Stream)
import OpenSolid.Stream qualified as Stream
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (HasUnits, SquareMeters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorCurve qualified as VectorCurve
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D qualified as VectorCurve2D
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D

data Curve1D units = Curve1D {compiled :: Compiled units, derivative :: ~(Curve1D units)}

type Compiled units = CompiledFunction Number (Quantity units) (Interval Unitless) (Interval units)

instance FFI (Curve1D Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve1D Meters) where
  representation = FFI.classRepresentation "LengthCurve"

instance FFI (Curve1D SquareMeters) where
  representation = FFI.classRepresentation "AreaCurve"

instance FFI (Curve1D Radians) where
  representation = FFI.classRepresentation "AngleCurve"

instance HasUnits (Curve1D units) units

instance Units.Coercion (Curve1D units1) (Curve1D units2) where
  coerce curve = Curve1D (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance ApproximateEquality (Curve1D units) units where
  curve1 ~= curve2 = do
    let equalPoints tValue = evaluate curve1 tValue ~= evaluate curve2 tValue
    NonEmpty.allSatisfy equalPoints Parameter.samples

instance
  units1 ~ units2 =>
  Intersects (Curve1D units1) (Quantity units2) units1
  where
  curve `intersects` value =
    -- TODO optimize this to use a special Solve1D.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and their exact locations)
    case zeros (curve - value) of
      Ok [] -> False
      Ok List.OneOrMore -> True
      Error IsZero -> True

instance
  units1 ~ units2 =>
  Intersects (Quantity units1) (Curve1D units2) units1
  where
  value `intersects` curve = curve `intersects` value

new :: Compiled units -> Curve1D units -> Curve1D units
new = Curve1D

concrete :: Expression Number (Quantity units) -> Curve1D units -> Curve1D units
concrete givenExpression givenDerivative =
  new (CompiledFunction.concrete givenExpression) givenDerivative

recursive :: Compiled units -> (Curve1D units -> Curve1D units) -> Curve1D units
recursive givenCompiled derivativeFunction =
  let result = Curve1D givenCompiled (derivativeFunction result) in result

-- | A curve equal to zero everywhere.
zero :: Curve1D units
zero = constant Quantity.zero

-- | Create a curve with the given constant value.
constant :: Quantity units -> Curve1D units
constant value = new (CompiledFunction.constant value) zero

{-| A curve parameter.

In other words, a curve whose value is equal to its input parameter.
When defining parametric curves, you will typically start with 'Curve1D.t'
and then use arithmetic operators etc. to build up more complex curves.
-}
t :: Curve1D Unitless
t = new (CompiledFunction.concrete Expression.t) (constant 1.0)

-- | Create a curve that linearly interpolates from the first value to the second.
interpolateFrom :: Quantity units -> Quantity units -> Curve1D units
interpolateFrom a b = a + t * (b - a)

compiled :: Curve1D units -> Compiled units
compiled = (.compiled)

-- | Get the derivative of a curve.
derivative :: Curve1D units -> Curve1D units
derivative = (.derivative)

instance Negation (Curve1D units) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (Curve1D units) (Curve1D units) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (Curve1D units) Sign (Curve1D units) where
  curve * Positive = curve
  curve * Negative = -curve

instance units1 ~ units2 => Addition (Curve1D units1) (Curve1D units2) (Curve1D units1) where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance units1 ~ units2 => Addition (Curve1D units1) (Quantity units2) (Curve1D units1) where
  curve + value = curve + constant value

instance units1 ~ units2 => Addition (Quantity units1) (Curve1D units2) (Curve1D units1) where
  value + curve = constant value + curve

instance units1 ~ units2 => Subtraction (Curve1D units1) (Curve1D units2) (Curve1D units1) where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  units1 ~ units2 =>
  Subtraction (Curve1D units1) (Quantity units2) (Curve1D units1)
  where
  curve - value = curve - constant value

instance
  units1 ~ units2 =>
  Subtraction (Quantity units1) (Curve1D units2) (Curve1D units1)
  where
  value - curve = constant value - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Curve1D units2) (Curve1D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance Multiplication_ (Curve1D units1) (Curve1D units2) (Curve1D (units1 ?*? units2)) where
  lhs ?*? rhs =
    new (lhs.compiled ?*? rhs.compiled) (lhs.derivative ?*? rhs + lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Quantity units2) (Curve1D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance Multiplication_ (Curve1D units1) (Quantity units2) (Curve1D (units1 ?*? units2)) where
  curve ?*? value = curve ?*? constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Curve1D units2) (Curve1D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance Multiplication_ (Quantity units1) (Curve1D units2) (Curve1D (units1 ?*? units2)) where
  value ?*? curve = constant value ?*? curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Vector2D units2 space) (VectorCurve2D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve1D units1)
    (Vector2D units2 space)
    (VectorCurve2D (units1 ?*? units2) space)
  where
  curve ?*? vector = curve ?*? VectorCurve2D.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2D units1 space) (Curve1D units2) (VectorCurve2D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Vector2D units1 space)
    (Curve1D units2)
    (VectorCurve2D (units1 ?*? units2) space)
  where
  vector ?*? curve = VectorCurve2D.constant vector ?*? curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Vector3D units2 space) (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve1D units1)
    (Vector3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  curve ?*? vector = curve ?*? VectorCurve3D.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3D units1 space) (Curve1D units2) (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Vector3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  vector ?*? curve = VectorCurve3D.constant vector ?*? curve

instance Composition (Curve1D Unitless) (Curve1D units) (Curve1D units) where
  f `compose` g =
    new (f.compiled `compose` g.compiled) ((f.derivative `compose` g) * g.derivative)

reverse :: Curve1D units -> Curve1D units
reverse curve = curve `compose` (1.0 - t)

bezier :: NonEmpty (Quantity units) -> Curve1D units
bezier controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezier (Bezier.derivative controlPoints))

hermite ::
  Quantity units ->
  List (Quantity units) ->
  Quantity units ->
  List (Quantity units) ->
  Curve1D units
hermite value0 derivatives0 value1 derivatives1 =
  bezier (Bezier.hermite value0 derivatives0 value1 derivatives1)

quadraticSpline :: Quantity units -> Quantity units -> Quantity units -> Curve1D units
quadraticSpline p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

cubicSpline :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Curve1D units
cubicSpline p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

rationalBezier ::
  Tolerance Unitless =>
  NonEmpty (Quantity units, Number) ->
  Result DivisionByZero (Curve1D units)
rationalBezier pointsAndWeights = do
  let scaledPoint (point, weight) = point * weight
  quotient
    (bezier (NonEmpty.map scaledPoint pointsAndWeights))
    (bezier (NonEmpty.map Pair.second pointsAndWeights))

rationalQuadraticSpline ::
  Tolerance Unitless =>
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  Result DivisionByZero (Curve1D units)
rationalQuadraticSpline pw1 pw2 pw3 = rationalBezier (NonEmpty.three pw1 pw2 pw3)

rationalCubicSpline ::
  Tolerance Unitless =>
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  (Quantity units, Number) ->
  Result DivisionByZero (Curve1D units)
rationalCubicSpline pw1 pw2 pw3 pw4 = rationalBezier (NonEmpty.four pw1 pw2 pw3 pw4)

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
{-# INLINE evaluate #-}
evaluate :: Curve1D units -> Number -> Quantity units
evaluate curve = CompiledFunction.evaluate curve.compiled

{-# INLINE evaluateAt #-}
evaluateAt :: Number -> Curve1D units -> Quantity units
evaluateAt tValue curve = evaluate curve tValue

{-# INLINE evaluateBounds #-}
evaluateBounds :: Curve1D units -> Interval Unitless -> Interval units
evaluateBounds curve = CompiledFunction.evaluateBounds curve.compiled

startValue :: Curve1D units -> Quantity units
startValue curve = evaluate curve 0.0

endValue :: Curve1D units -> Quantity units
endValue curve = evaluate curve 1.0

desingularize ::
  Maybe (Quantity units, Quantity units) ->
  Curve1D units ->
  Maybe (Quantity units, Quantity units) ->
  Curve1D units
desingularize = VectorCurve.desingularize

desingularized :: Curve1D units -> Curve1D units -> Curve1D units -> Curve1D units
desingularized start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (desingularized start.derivative middle.derivative end.derivative)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  Curve1D units1 ->
  Curve1D units2 ->
  Result DivisionByZero (Curve1D units3)
quotient lhs rhs = Result.map Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  Curve1D units1 ->
  Curve1D units2 ->
  Result DivisionByZero (Curve1D (units1 ?/? units2))
quotient_ lhs rhs =
  if rhs ~= zero
    then Error DivisionByZero
    else Ok (lhs ?/? WithNoInteriorZeros rhs)

newtype WithNoZeros units = WithNoZeros (Curve1D units)

instance HasUnits (WithNoZeros units) units

instance Units.Coercion (WithNoZeros units1) (WithNoZeros units2) where
  coerce (WithNoZeros curve) = WithNoZeros (Units.coerce curve)

instance Division_ (Curve1D units1) (WithNoZeros units2) (Curve1D (units1 ?/? units2)) where
  lhs ?/? rhsWithNoZeros = do
    let rhs = Curve1D.WithNoZeros.unwrap rhsWithNoZeros
    let quotientCompiled = compiled lhs ?/? compiled rhs
    let quotientDerivative = Units.simplify do
          (derivative lhs ?*? rhs - lhs ?*? derivative rhs)
            ?/? Curve1D.WithNoZeros.squared_ rhsWithNoZeros
    new quotientCompiled quotientDerivative

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1D units1) (WithNoZeros units2) (Curve1D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

newtype WithNoInteriorZeros units = WithNoInteriorZeros (Curve1D units)

instance HasUnits (WithNoInteriorZeros units) units

instance Units.Coercion (WithNoInteriorZeros units1) (WithNoInteriorZeros units2) where
  coerce (WithNoInteriorZeros curve) = WithNoInteriorZeros (Units.coerce curve)

instance Division_ (Curve1D units1) (WithNoInteriorZeros units2) (Curve1D (units1 ?/? units2)) where
  (?/?) = VectorCurve.desingularizedQuotient

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1D units1) (WithNoInteriorZeros units2) (Curve1D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1D units1) (Quantity units2) (Curve1D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance Division_ (Curve1D units1) (Quantity units2) (Curve1D (units1 ?/? units2)) where
  curve ?/? value = Units.simplify (curve ?*? (1.0 ?/? value))

-- | Compute the square of a curve.
squared :: Units.Squared units1 units2 => Curve1D units1 -> Curve1D units2
squared curve = Units.specialize (squared_ curve)

squared_ :: Curve1D units -> Curve1D (units ?*? units)
squared_ curve =
  new
    (CompiledFunction.map Expression.squared_ Quantity.squared_ Interval.squared_ curve.compiled)
    (2.0 * curve ?*? curve.derivative)

-- | Compute the square root of a curve.
sqrt :: Tolerance units1 => Units.Squared units1 units2 => Curve1D units2 -> Curve1D units1
sqrt curve = sqrt_ (Units.unspecialize curve)

sqrt_ :: Tolerance units => Curve1D (units ?*? units) -> Curve1D units
sqrt_ curve =
  if Tolerance.using (Quantity.squared_ ?tolerance) (curve ~= zero)
    then zero
    else
      Curve1D.WithNoInteriorZeros.unwrap $
        Curve1D.WithNoInteriorZeros.sqrt_ (WithNoInteriorZeros curve)

-- | Compute the cube of a curve.
cubed :: Curve1D Unitless -> Curve1D Unitless
cubed curve =
  new
    (CompiledFunction.map Expression.cubed Number.cubed Interval.cubed curve.compiled)
    (3.0 * squared curve * curve.derivative)

-- | Compute the sine of a curve.
sin :: Curve1D Radians -> Curve1D Unitless
sin curve =
  new
    (CompiledFunction.map Expression.sin Angle.sin Interval.sin curve.compiled)
    (cos curve * (curve.derivative / Angle.radian))

-- | Compute the cosine of a curve.
cos :: Curve1D Radians -> Curve1D Unitless
cos curve =
  new
    (CompiledFunction.map Expression.cos Angle.cos Interval.cos curve.compiled)
    (negate (sin curve) * (curve.derivative / Angle.radian))

integrate :: Curve1D units -> Estimate units
integrate curve = Estimate.new (Integral curve curve.derivative Interval.unit)

data Integral units = Integral (Curve1D units) (Curve1D units) (Interval Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral curve curveDerivative domain) = do
    let dx = Interval.width domain
    let derivativeBounds = evaluateBounds curveDerivative domain
    let estimate0 = dx * evaluateBounds curve domain
    let y1 = evaluate curve (Interval.lower domain)
    let y2 = evaluate curve (Interval.upper domain)
    let m = Interval.width derivativeBounds
    let error1 = 0.125 * m * dx * dx
    let estimate1 = dx * Quantity.midpoint y1 y2 + Interval -error1 error1
    case Interval.intersection estimate0 estimate1 of
      Just intersection -> intersection
      Nothing -> estimate0 -- Shouldn't happen if bounds are correct

  refineImpl (Integral curve curveDerivative domain) = do
    let (leftDomain, rightDomain) = Interval.bisect domain
    let leftIntegral = Integral curve curveDerivative leftDomain
    let rightIntegral = Integral curve curveDerivative rightDomain
    Estimate.new leftIntegral + Estimate.new rightIntegral

singularityTolerance :: Curve1D units -> Quantity units
singularityTolerance curve =
  1e-9 * NonEmpty.maximumOf (Quantity.abs . evaluate curve) Parameter.samples

----- ZERO FINDING -----

data IsZero = IsZero deriving (Eq, Show)

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
zeros :: Tolerance units => Curve1D units -> Result IsZero (List Zero)
zeros curve
  | curve ~= zero = Error IsZero
  | otherwise = do
      let derivatives = Stream.iterate (.derivative) curve
      let derivativeBounds tBounds = Stream.map (\f -> evaluateBounds f tBounds) derivatives
      let cache = Solve1D.init derivativeBounds
      case Solve1D.search (findZeros derivatives) cache of
        Ok foundZeros -> Ok (List.sortBy (.location) foundZeros)
        Error Solve1D.InfiniteRecursion -> throw HigherOrderZero

findZeros ::
  Tolerance units =>
  Stream (Curve1D units) ->
  Domain1D ->
  Stream (Interval units) ->
  Solve1D.Exclusions exclusions ->
  Solve1D.Action exclusions Zero
findZeros derivatives subdomain derivativeBounds exclusions
  -- Skip the subdomain entirely if the curve itself is non-zero everywhere
  | not (Stream.head derivativeBounds `intersects` Quantity.zero) = Solve1D.pass
  -- Optimization heuristic: bisect down to small subdomains first,
  -- to quickly eliminate most of the curve based on simple value bounds
  -- before attempting more complex/sophisticated solving
  | Interval.width (Domain1D.bounds subdomain) > 1 / 1024 = Solve1D.recurse
  | otherwise = case exclusions of
      Solve1D.SomeExclusions -> Solve1D.recurse
      Solve1D.NoExclusions ->
        case findZerosOrder 0 derivatives subdomain derivativeBounds of
          Unresolved -> Solve1D.recurse
          Resolved [] -> Solve1D.pass
          Resolved (NonEmpty subdomainZeros) -> do
            let subdomainInterior = Domain1D.interior subdomain
            let isInterior (t0, _) = Interval.includes t0 subdomainInterior
            if NonEmpty.allSatisfy isInterior subdomainZeros
              then Solve1D.return (NonEmpty.map toZero subdomainZeros)
              else Solve1D.recurse

toZero :: (Number, Solve1D.Neighborhood units) -> Zero
toZero (t0, neighborhood) = Solve1D.zero t0 neighborhood

maxZeroOrder :: Int
maxZeroOrder = 3

findZerosOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve1D units) ->
  Domain1D ->
  Stream (Interval units) ->
  Fuzzy (List (Number, Solve1D.Neighborhood units))
findZerosOrder k derivatives subdomain derivativeBounds
  -- A derivative is resolved, so it has no zeros
  -- (note that if k == 0, we already checked for the curve being non-zero in findZeros above)
  | k > 0 && Interval.isResolved (Stream.head derivativeBounds) = Resolved []
  -- We've exceeded the maximum zero order without finding a non-zero derivative
  | k > maxZeroOrder = Unresolved
  -- Otherwise, find higher-order zeros and then search in between them
  | otherwise = do
      let higherDerivatives = Stream.tail derivatives
      let higherDerivativeBounds = Stream.tail derivativeBounds
      let currentDerivative = Stream.head derivatives
      let nextDerivative = Stream.head higherDerivatives
      let tBounds = Domain1D.bounds subdomain
      higherOrderZeros <- findZerosOrder (k + 1) higherDerivatives subdomain higherDerivativeBounds
      case higherOrderZeros of
        [] -> solveMonotonic k currentDerivative nextDerivative tBounds
        List.One (t0, neighborhood) -> do
          if Quantity.abs (evaluate currentDerivative t0)
            <= Solve1D.derivativeTolerance neighborhood k
            then Resolved [(t0, neighborhood)]
            else do
              let leftBounds = Interval (Interval.lower tBounds) t0
              let rightBounds = Interval t0 (Interval.upper tBounds)
              leftZeros <- solveMonotonic k currentDerivative nextDerivative leftBounds
              rightZeros <- solveMonotonic k currentDerivative nextDerivative rightBounds
              Resolved (leftZeros <> rightZeros)
        List.TwoOrMore -> Unresolved

solveMonotonic ::
  Tolerance units =>
  Int ->
  Curve1D units ->
  Curve1D units ->
  Interval Unitless ->
  Fuzzy (List (Number, Solve1D.Neighborhood units))
solveMonotonic m fm fn tBounds = do
  let n = m + 1
  let Interval tLow tHigh = tBounds
  let startNeighborhood = Solve1D.neighborhood n (evaluate fn tLow)
  if Quantity.abs (evaluate fm tLow) <= Solve1D.derivativeTolerance startNeighborhood m
    then if tLow == 0.0 then Resolved [(0.0, startNeighborhood)] else Unresolved
    else do
      let endNeighborhood = Solve1D.neighborhood n (evaluate fn tHigh)
      if Quantity.abs (evaluate fm tHigh) <= Solve1D.derivativeTolerance endNeighborhood m
        then if tHigh == 1.0 then Resolved [(1.0, endNeighborhood)] else Unresolved
        else do
          case Solve1D.monotonic (evaluate fm) (evaluate fn) tBounds of
            Solve1D.Exact t0 -> Resolved [(t0, Solve1D.neighborhood n (evaluate fn t0))]
            Solve1D.Closest _ -> Unresolved

data CrossesZero = CrossesZero deriving (Eq, Show)

{-| Attempt to find the (consistent) sign of all values on the curve.

Will return an error if the curve crosses zero,
or has an indeterminate higher-order zero anywhere.
If the curve is zero everywhere, then returns positive.
-}
sign :: Tolerance units => Curve1D units -> Result CrossesZero Sign
sign curve = case zeros curve of
  Error IsZero -> Ok Positive
  Ok curveZeros ->
    case List.filter isInnerZero curveZeros of
      [] -> Ok (Quantity.sign (evaluate curve 0.5)) -- No inner zeros, so check sign at t=0.5
      NonEmpty innerZeros ->
        case NonEmpty.filter isCrossingZero innerZeros of
          List.OneOrMore -> Error CrossesZero -- There exists at least one inner crossing zero
          [] -> do
            -- All inner zeros are non-crossing (e.g. quadratic) ones,
            -- so we can safely test the curve
            -- halfway between t=0 and the first inner zero
            let firstInnerZero = NonEmpty.first innerZeros
            let testPoint = 0.5 * firstInnerZero.location
            Ok (Quantity.sign (evaluate curve testPoint))

isInnerZero :: Zero -> Bool
isInnerZero curveZero = not (Parameter.isEndpoint curveZero.location)

isCrossingZero :: Zero -> Bool
isCrossingZero curveZero =
  -- Curve1D order 0 is linear (crossing) zero
  -- Curve1D order 1 is quadratic (non-crossing) zero
  -- Curve1D order 2 is cubic (crossing) zero
  -- Curve1D order 3 is quartic (non-crossing) zero, etc.
  Int.isEven curveZero.order

b00 :: Curve1D Unitless
b00 =
  concrete Expression.b00 $
    concrete Expression.b00d1 $
      concrete Expression.b00d2 $
        concrete Expression.b00d3 $
          constant 72.0

b01 :: Curve1D Unitless
b01 =
  concrete Expression.b01 $
    concrete Expression.b01d1 $
      concrete Expression.b01d2 $
        concrete Expression.b01d3 $
          constant 48.0

b02 :: Curve1D Unitless
b02 =
  concrete Expression.b02 $
    concrete Expression.b02d1 $
      concrete Expression.b02d2 $
        concrete Expression.b02d3 $
          constant 12.0

b10 :: Curve1D Unitless
b10 =
  concrete Expression.b10 $
    concrete Expression.b10d1 $
      concrete Expression.b10d2 $
        concrete Expression.b10d3 $
          constant -72.0

b11 :: Curve1D Unitless
b11 =
  concrete Expression.b11 $
    concrete Expression.b11d1 $
      concrete Expression.b11d2 $
        concrete Expression.b11d3 $
          constant 24.0

newtonRaphson :: Curve1D units -> Number -> Number
newtonRaphson curve tValue =
  NewtonRaphson1D.curve (evaluate curve) (evaluate (derivative curve)) tValue
