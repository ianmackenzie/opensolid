module OpenSolid.Curve
  ( Curve
  , Compiled
  , Zero
  , isEndpoint
  , evaluate
  , evaluateBounds
  , new
  , recursive
  , zero
  , constant
  , t
  , line
  , bezier
  , hermite
  , quadraticSpline
  , cubicSpline
  , rationalBezier
  , rationalQuadraticSpline
  , rationalCubicSpline
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  , ZeroEverywhere (ZeroEverywhere)
  , zeros
  , CrossesZero (CrossesZero)
  , sign
  , reverse
  , integrate
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
import OpenSolid.Domain1d (Domain1d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Error qualified as Error
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
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

data Curve units where
  Curve :: Compiled units -> ~(Curve units) -> Curve units

type Compiled units = CompiledFunction Float (Qty units) (Bounds Unitless) (Bounds units)

instance HasField "compiled" (Curve units) (Compiled units) where
  getField (Curve c _) = c

instance HasField "derivative" (Curve units) (Curve units) where
  getField (Curve _ d) = d

instance FFI (Curve Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve Meters) where
  representation = FFI.classRepresentation "LengthCurve"

instance FFI (Curve SquareMeters) where
  representation = FFI.classRepresentation "AreaCurve"

instance FFI (Curve Radians) where
  representation = FFI.classRepresentation "AngleCurve"

instance HasUnits (Curve units) units (Curve Unitless)

instance Units.Coercion (Curve units1) (Curve units2) where
  coerce curve = Curve (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Curve units2) units1
  where
  curve1 ~= curve2 =
    List.allTrue [evaluate curve1 tValue ~= evaluate curve2 tValue | tValue <- Parameter.samples]

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Qty units2) units1
  where
  curve ~= value = List.allTrue [evaluate curve tValue ~= value | tValue <- Parameter.samples]

instance
  units1 ~ units2 =>
  Intersects (Curve units1) (Qty units2) units1
  where
  curve ^ value =
    -- TODO optimize this to use a special Solve1d.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and their exact locations)
    case zeros (curve - value) of
      Success [] -> False
      Success List.OneOrMore -> True
      Failure ZeroEverywhere -> True

instance
  units1 ~ units2 =>
  Intersects (Qty units1) (Curve units2) units1
  where
  value ^ curve = curve ^ value

isEndpoint :: Float -> Bool
isEndpoint tValue = tValue == 0.0 || tValue == 1.0

new :: Compiled units -> Curve units -> Curve units
new = Curve

recursive :: Compiled units -> (Curve units -> Curve units) -> Curve units
recursive givenCompiled derivativeFunction =
  let result = Curve givenCompiled (derivativeFunction result) in result

-- | A curve equal to zero everywhere.
zero :: Curve units
zero = constant Qty.zero

-- | Create a curve with the given constant value.
constant :: Qty units -> Curve units
constant value = new (CompiledFunction.constant value) zero

{-| A curve parameter.

In other words, a curve whose value is equal to its input parameter.
When defining parametric curves, you will typically start with 'Curve.t'
and then use arithmetic operators etc. to build up more complex curves.
-}
t :: Curve Unitless
t = new (CompiledFunction.concrete Expression.t) (constant 1.0)

-- | Create a curve that linearly interpolates from the first value to the second.
line :: Qty units -> Qty units -> Curve units
line a b = a + t * (b - a)

instance Negation (Curve units) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (Curve units) (Curve units) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (Curve units) Sign (Curve units) where
  curve * Positive = curve
  curve * Negative = -curve

instance units1 ~ units2 => Addition (Curve units1) (Curve units2) (Curve units1) where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance units1 ~ units2 => Addition (Curve units1) (Qty units2) (Curve units1) where
  curve + value = curve + constant value

instance units1 ~ units2 => Addition (Qty units1) (Curve units2) (Curve units1) where
  value + curve = constant value + curve

instance units1 ~ units2 => Subtraction (Curve units1) (Curve units2) (Curve units1) where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  units1 ~ units2 =>
  Subtraction (Curve units1) (Qty units2) (Curve units1)
  where
  curve - value = curve - constant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Curve units2) (Curve units1)
  where
  value - curve = constant value - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Curve units2) (Curve units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance Multiplication' (Curve units1) (Curve units2) (Curve (units1 :*: units2)) where
  lhs .*. rhs =
    new (lhs.compiled .*. rhs.compiled) (lhs.derivative .*. rhs + lhs .*. rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Qty units2) (Curve units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance Multiplication' (Curve units1) (Qty units2) (Curve (units1 :*: units2)) where
  curve .*. value = curve .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Curve units2) (Curve units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance Multiplication' (Qty units1) (Curve units2) (Curve (units1 :*: units2)) where
  value .*. curve = constant value .*. curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Vector2d (space @ units2)) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve units1)
    (Vector2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  curve .*. vector = curve .*. VectorCurve2d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Curve units2) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Vector2d (space @ units1))
    (Curve units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  vector .*. curve = VectorCurve2d.constant vector .*. curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (Vector3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve units1)
    (Vector3d (space @ units2))
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  curve .*. vector = curve .*. VectorCurve3d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Vector3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  vector .*. curve = VectorCurve3d.constant vector .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve units1) (Curve units2) (Curve units3)
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance Division' (Curve units1) (Curve units2) (Curve (units1 :/: units2)) where
  lhs ./. rhs =
    recursive
      @ lhs.compiled ./. rhs.compiled
      @ \self -> lhs.derivative ./. rhs - self * (rhs.derivative / rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve units1) (Qty units2) (Curve units3)
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance Division' (Curve units1) (Qty units2) (Curve (units1 :/: units2)) where
  curve ./. value = curve ./. constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Curve units2) (Curve units3)
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance Division' (Qty units1) (Curve units2) (Curve (units1 :/: units2)) where
  value ./. curve = constant value ./. curve

instance Composition (Curve Unitless) (Curve units) (Curve units) where
  f . g = new (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

reverse :: Curve units -> Curve units
reverse curve = curve . (1.0 - t)

bezier :: NonEmpty (Qty units) -> Curve units
bezier controlPoints = do
  let compiledBezier = CompiledFunction.concrete (Expression.bezierCurve controlPoints)
  let derivativeControlPoints = Bezier.derivative controlPoints
  new compiledBezier (bezier derivativeControlPoints)

hermite :: Qty units -> List (Qty units) -> Qty units -> List (Qty units) -> Curve units
hermite startValue startDerivatives endValue endDerivatives =
  bezier (Bezier.hermite startValue startDerivatives endValue endDerivatives)

quadraticSpline :: Qty units -> Qty units -> Qty units -> Curve units
quadraticSpline p1 p2 p3 = bezier (NonEmpty.three p1 p2 p3)

cubicSpline :: Qty units -> Qty units -> Qty units -> Qty units -> Curve units
cubicSpline p1 p2 p3 p4 = bezier (NonEmpty.four p1 p2 p3 p4)

rationalBezier :: NonEmpty (Qty units, Float) -> Curve units
rationalBezier pointsAndWeights = do
  let scaledPoint (point, weight) = point * weight
  bezier (NonEmpty.map scaledPoint pointsAndWeights)
    / bezier (NonEmpty.map Pair.second pointsAndWeights)

rationalQuadraticSpline ::
  (Qty units, Float) ->
  (Qty units, Float) ->
  (Qty units, Float) ->
  Curve units
rationalQuadraticSpline pw1 pw2 pw3 = rationalBezier (NonEmpty.three pw1 pw2 pw3)

rationalCubicSpline ::
  (Qty units, Float) ->
  (Qty units, Float) ->
  (Qty units, Float) ->
  (Qty units, Float) ->
  Curve units
rationalCubicSpline pw1 pw2 pw3 pw4 = rationalBezier (NonEmpty.four pw1 pw2 pw3 pw4)

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Curve units -> Float -> Qty units
evaluate curve = CompiledFunction.evaluate curve.compiled

evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
evaluateBounds curve = CompiledFunction.evaluateBounds curve.compiled

-- | Compute the square of a curve.
squared :: Units.Squared units1 units2 => Curve units1 -> Curve units2
squared curve = Units.specialize (squared' curve)

squared' :: Curve units -> Curve (units :*: units)
squared' curve =
  new
    (CompiledFunction.map Expression.squared' Qty.squared' Bounds.squared' curve.compiled)
    (2.0 * curve .*. curve.derivative)

-- | Compute the square root of a curve.
sqrt :: Tolerance units1 => Units.Squared units1 units2 => Curve units2 -> Curve units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Tolerance units => Curve (units :*: units) -> Curve units
sqrt' curve =
  if Tolerance.using Tolerance.squared' (curve ~= Qty.zero)
    then zero
    else
      recursive
        (CompiledFunction.map Expression.sqrt' Qty.sqrt' Bounds.sqrt' curve.compiled)
        (\self -> curve.derivative .!/! (2.0 * self))

-- | Compute the sine of a curve.
sin :: Curve Radians -> Curve Unitless
sin curve =
  new
    (CompiledFunction.map Expression.sin Angle.sin Bounds.sin curve.compiled)
    (cos curve * (curve.derivative / Angle.radian))

-- | Compute the cosine of a curve.
cos :: Curve Radians -> Curve Unitless
cos curve =
  new
    (CompiledFunction.map Expression.cos Angle.cos Bounds.cos curve.compiled)
    (negate (sin curve) * (curve.derivative / Angle.radian))

integrate :: Curve units -> Estimate units
integrate curve = Estimate.new (Integral curve curve.derivative Bounds.unitInterval)

data Integral units = Integral (Curve units) (Curve units) (Bounds Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral curve curveDerivative domain) = do
    let dx = Bounds.width domain
    let derivativeBounds = evaluateBounds curveDerivative domain
    let estimate0 = dx * evaluateBounds curve domain
    let y1 = evaluate curve (Bounds.lower domain)
    let y2 = evaluate curve (Bounds.upper domain)
    let m = Bounds.width derivativeBounds
    let error1 = 0.125 * m * dx * dx
    let estimate1 = dx * Qty.midpoint y1 y2 + Bounds -error1 error1
    case Bounds.intersection estimate0 estimate1 of
      Just intersection -> intersection
      Nothing -> estimate0 -- Shouldn't happen if bounds are correct

  refineImpl (Integral curve curveDerivative domain) = do
    let (leftDomain, rightDomain) = Bounds.bisect domain
    let leftIntegral = Integral curve curveDerivative leftDomain
    let rightIntegral = Integral curve curveDerivative rightDomain
    Estimate.new leftIntegral + Estimate.new rightIntegral

----- ZERO FINDING -----

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, Error.Message)

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
zeros :: Tolerance units => Curve units -> Result ZeroEverywhere (List Zero)
zeros curve
  | curve ~= Qty.zero = Failure ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Stream.iterate (.derivative) curve
      let derivativeBounds tBounds = Stream.map (\f -> evaluateBounds f tBounds) derivatives
      let cache = Solve1d.init derivativeBounds
      case Solve1d.search (findZeros derivatives) cache of
        Success foundZeros -> Success (List.sortBy (.location) foundZeros)
        Failure Solve1d.InfiniteRecursion -> exception "Higher-order zero detected"

findZeros ::
  Tolerance units =>
  Stream (Curve units) ->
  Domain1d ->
  Stream (Bounds units) ->
  Solve1d.Exclusions exclusions ->
  Solve1d.Action exclusions Zero
findZeros derivatives subdomain derivativeBounds exclusions
  -- Skip the subdomain entirely if the curve itself is non-zero everywhere
  | not (Stream.head derivativeBounds ^ Qty.zero) = Solve1d.pass
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
            if NonEmpty.allSatisfy (\(t0, _) -> Bounds.includes t0 subdomainInterior) subdomainZeros
              then Solve1d.return (NonEmpty.map toZero subdomainZeros)
              else Solve1d.recurse

toZero :: (Float, Solve1d.Neighborhood units) -> Zero
toZero (t0, neighborhood) = Solve1d.zero t0 neighborhood

maxZeroOrder :: Int
maxZeroOrder = 3

findZerosOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve units) ->
  Domain1d ->
  Stream (Bounds units) ->
  Fuzzy (List (Float, Solve1d.Neighborhood units))
findZerosOrder k derivatives subdomain derivativeBounds
  -- A derivative is resolved, so it has no zeros
  -- (note that if k == 0, we already checked for the curve being non-zero in findZeros above)
  | k > 0 && Bounds.isResolved (Stream.head derivativeBounds) = Resolved []
  -- We've exceeded the maximum zero order without finding a non-zero derivative
  | k > maxZeroOrder = Unresolved
  -- Otherwise, find higher-order zeros and then search in between them
  | otherwise = Fuzzy.do
      let higherDerivatives = Stream.tail derivatives
      let higherDerivativeBounds = Stream.tail derivativeBounds
      let currentDerivative = Stream.head derivatives
      let nextDerivative = Stream.head higherDerivatives
      let tBounds = Domain1d.bounds subdomain
      higherOrderZeros <- findZerosOrder (k + 1) higherDerivatives subdomain higherDerivativeBounds
      case higherOrderZeros of
        [] -> solveMonotonic k currentDerivative nextDerivative tBounds
        List.One (t0, neighborhood) -> do
          if Qty.abs (evaluate currentDerivative t0) <= Solve1d.derivativeTolerance neighborhood k
            then Resolved [(t0, neighborhood)]
            else Fuzzy.do
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
  Fuzzy (List (Float, Solve1d.Neighborhood units))
solveMonotonic m fm fn tBounds = do
  let n = m + 1
  let Bounds tLow tHigh = tBounds
  let startNeighborhood = Solve1d.neighborhood n (evaluate fn tLow)
  if Qty.abs (evaluate fm tLow) <= Solve1d.derivativeTolerance startNeighborhood m
    then if tLow == 0.0 then Resolved [(0.0, startNeighborhood)] else Unresolved
    else do
      let endNeighborhood = Solve1d.neighborhood n (evaluate fn tHigh)
      if Qty.abs (evaluate fm tHigh) <= Solve1d.derivativeTolerance endNeighborhood m
        then if tHigh == 1.0 then Resolved [(1.0, endNeighborhood)] else Unresolved
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
  Failure ZeroEverywhere -> Success Positive
  Success curveZeros ->
    case List.filter isInnerZero curveZeros of
      [] -> Success (Qty.sign (evaluate curve 0.5)) -- No inner zeros, so check sign at t=0.5
      NonEmpty innerZeros ->
        case NonEmpty.filter isCrossingZero innerZeros of
          List.OneOrMore -> Failure CrossesZero -- There exists at least one inner crossing zero
          [] -> do
            -- All inner zeros are non-crossing (e.g. quadratic) ones,
            -- so we can safely test the curve
            -- halfway between t=0 and the first inner zero
            let testPoint = 0.5 * innerZeros.first.location
            Success (Qty.sign (evaluate curve testPoint))

isInnerZero :: Zero -> Bool
isInnerZero curveZero = not (isEndpoint curveZero.location)

isCrossingZero :: Zero -> Bool
isCrossingZero curveZero =
  -- Curve order 0 is linear (crossing) zero
  -- Curve order 1 is quadratic (non-crossing) zero
  -- Curve order 2 is cubic (crossing) zero
  -- Curve order 3 is quartic (non-crossing) zero, etc.
  Int.isEven curveZero.order
