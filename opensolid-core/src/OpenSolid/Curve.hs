module OpenSolid.Curve
  ( Curve
  , Zero
  , Interface (..)
  , isEndpoint
  , evaluate
  , evaluateBounds
  , compiled
  , derivative
  , new
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
  , zeros
  , reverse
  , integral
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve.Zero (Zero)
import OpenSolid.Curve.Zero qualified as Zero
import OpenSolid.Curve.Zeros qualified as Zeros
import OpenSolid.Domain1d (Domain1d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Solve1d qualified as Solve1d
import OpenSolid.Stream (Stream)
import OpenSolid.Stream qualified as Stream
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (Meters, Radians, SquareMeters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d qualified as VectorCurve2d
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import Prelude qualified

data Curve units where
  Curve ::
    { compiled :: Compiled units
    , derivative :: ~(Curve units)
    , shown :: ~(List Char)
    } ->
    Curve units

type Compiled units = CompiledFunction Float (Qty units) (Range Unitless) (Range units)

class
  Show curve =>
  Interface curve units
    | curve -> units
  where
  compileImpl :: curve -> Compiled units
  derivativeImpl :: Curve units -> curve -> Curve units

instance Show (Curve units) where
  show Curve{shown} = shown

instance FFI (Curve Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve Meters) where
  representation = FFI.classRepresentation "LengthCurve"

instance FFI (Curve SquareMeters) where
  representation = FFI.classRepresentation "AreaCurve"

instance FFI (Curve Radians) where
  representation = FFI.classRepresentation "AngleCurve"

instance HasUnits (Curve units) units (Curve Unitless)

instance Units.Coercion (Curve unitsA) (Curve unitsB) where
  coerce Curve{compiled, derivative, shown} =
    Curve
      { compiled = Units.coerce compiled
      , derivative = Units.coerce derivative
      , shown = shown
      }

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve units1) (Curve units2) units1
  where
  curve1 ~= curve2 = curve1 - curve2 ~= Qty.zero

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
      Failure Zeros.ZeroEverywhere -> True
      Failure Zeros.HigherOrderZero -> True

instance
  units1 ~ units2 =>
  Intersects (Qty units1) (Curve units2) units1
  where
  value ^ curve = curve ^ value

isEndpoint :: Float -> Bool
isEndpoint tValue = tValue == 0.0 || tValue == 1.0

new :: Interface curve units => curve -> Curve units
new curve = do
  let result =
        Curve
          { compiled = compileImpl curve
          , derivative = derivativeImpl result curve
          , shown = Prelude.show curve
          }
  result

instance Interface (Qty units) units where
  compileImpl value = CompiledFunction.constant value
  derivativeImpl _ _ = zero

-- | A curve equal to zero everywhere.
zero :: Curve units
zero = constant Qty.zero

-- | Create a curve with the given constant value.
constant :: Qty units -> Curve units
constant = new

{-| A curve parameter.

In other words, a curve whose value is equal to its input parameter.
When defining parametric curves, you will typically start with 'Curve.t'
and then use arithmetic operators etc. to build up more complex curves.
-}
t :: Curve Unitless
t = new Parameter

data Parameter = Parameter deriving (Show)

instance Interface Parameter Unitless where
  compileImpl Parameter = CompiledFunction.concrete Expression.t
  derivativeImpl _ Parameter = constant 1.0

-- | Create a curve that linearly interpolates from the first value to the second.
line :: Qty units -> Qty units -> Curve units
line a b = a + t * (b - a)

instance Negation (Curve units) where
  negate = new . Negated

newtype Negated units = Negated (Curve units) deriving (Show)

instance Interface (Negated units) units where
  compileImpl (Negated curve) = negate (compiled curve)
  derivativeImpl _ (Negated curve) = negate (derivative curve)

instance Multiplication Sign (Curve units) (Curve units) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (Curve units) Sign (Curve units) where
  curve * Positive = curve
  curve * Negative = -curve

instance units ~ units_ => Addition (Curve units) (Curve units_) (Curve units) where
  lhs + rhs = new (lhs :+: rhs)

instance units1 ~ units2 => Interface (Curve units1 :+: Curve units2) units1 where
  compileImpl (f1 :+: f2) = compiled f1 + compiled f2
  derivativeImpl _ (f1 :+: f2) = derivative f1 + derivative f2

instance units ~ units_ => Addition (Curve units) (Qty units_) (Curve units) where
  curve + value = curve + constant value

instance units ~ units_ => Addition (Qty units) (Curve units_) (Curve units) where
  value + curve = constant value + curve

instance units1 ~ units2 => Subtraction (Curve units1) (Curve units2) (Curve units1) where
  lhs - rhs = new (lhs :-: rhs)

instance units1 ~ units2 => Interface (Curve units1 :-: Curve units2) units1 where
  compileImpl (f1 :-: f2) = compiled f1 - compiled f2
  derivativeImpl _ (f1 :-: f2) = derivative f1 - derivative f2

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
  lhs .*. rhs = new (lhs :*: rhs)

instance Interface (Curve units1 :*: Curve units2) (units1 :*: units2) where
  compileImpl (c1 :*: c2) = compiled c1 .*. compiled c2
  derivativeImpl _ (c1 :*: c2) = derivative c1 .*. c2 + c1 .*. derivative c2

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
  lhs ./. rhs = new (lhs :/: rhs)

instance Interface (Curve units1 :/: Curve units2) (units1 :/: units2) where
  compileImpl (c1 :/: c2) = compiled c1 ./. compiled c2
  derivativeImpl self (c1 :/: c2) = derivative c1 ./. c2 + self * (derivative c2 / c2)

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

instance unitless ~ Unitless => Composition (Curve unitless) (Curve units) (Curve units) where
  outer . inner = new (outer :.: inner)

instance unitless ~ Unitless => Interface (Curve units :.: Curve unitless) units where
  compileImpl (outer :.: inner) =
    compiled outer . compiled inner

  derivativeImpl _ (outer :.: inner) =
    (derivative outer . inner) * derivative inner

reverse :: Curve units -> Curve units
reverse curve = curve . (1.0 - t)

bezier :: NonEmpty (Qty units) -> Curve units
bezier = new . Bezier

newtype Bezier units = Bezier (NonEmpty (Qty units)) deriving (Show)

instance Interface (Bezier units) units where
  compileImpl (Bezier controlPoints) =
    CompiledFunction.concrete (Expression.bezierCurve controlPoints)

  derivativeImpl _ (Bezier controlPoints) = do
    let scale = Float.int (NonEmpty.length controlPoints - 1)
    let scaledDifference p1 p2 = scale * (p2 - p1)
    let scaledDifferences = NonEmpty.successive scaledDifference controlPoints
    case scaledDifferences of
      [] -> zero
      NonEmpty derivativeControlPoints -> bezier derivativeControlPoints

hermite :: (Qty units, List (Qty units)) -> (Qty units, List (Qty units)) -> Curve units
hermite (startValue, startDerivatives) (endValue, endDerivatives) = do
  let numStartDerivatives = List.length startDerivatives
  let numEndDerivatives = List.length endDerivatives
  let curveDegree = Float.int (1 + numStartDerivatives + numEndDerivatives)
  let scaledStartDerivatives = scaleDerivatives Positive 1.0 curveDegree startDerivatives
  let scaledEndDerivatives = scaleDerivatives Negative 1.0 curveDegree endDerivatives
  let startControlPoints =
        derivedControlPoints startValue 1 (numStartDerivatives + 1) scaledStartDerivatives
  let endControlPoints =
        List.reverse $
          derivedControlPoints endValue 1 (numEndDerivatives + 1) scaledEndDerivatives
  let controlPoints = startValue :| (startControlPoints + endControlPoints + [endValue])
  bezier controlPoints

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

scaleDerivatives :: Sign -> Float -> Float -> List (Qty units) -> List (Qty units)
scaleDerivatives _ _ _ [] = []
scaleDerivatives sign scale n (first : rest) = do
  let updatedScale = sign * scale / n
  updatedScale * first : scaleDerivatives sign updatedScale (n - 1.0) rest

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Curve units -> Float -> Qty units
evaluate Curve{compiled} = CompiledFunction.evaluate compiled

evaluateBounds :: Curve units -> Range Unitless -> Range units
evaluateBounds Curve{compiled} = CompiledFunction.evaluateBounds compiled

offset :: Int -> List (Qty units) -> Qty units
offset i scaledDerivatives =
  Qty.sum $
    List.mapWithIndex (\j q -> Float.int (Int.choose (i - 1) j) * q) $
      List.take i scaledDerivatives

derivedControlPoints :: Qty units -> Int -> Int -> List (Qty units) -> List (Qty units)
derivedControlPoints previousPoint i n qs
  | i < n = do
      let newPoint = previousPoint + offset i qs
      newPoint : derivedControlPoints newPoint (i + 1) n qs
  | otherwise = []

-- | Compute the square of a curve.
squared :: Units.Squared units1 units2 => Curve units1 -> Curve units2
squared curve = Units.specialize (squared' curve)

squared' :: Curve units -> Curve (units :*: units)
squared' = new . Squared'

newtype Squared' units = Squared' (Curve units) deriving (Show)

instance Interface (Squared' units) (units :*: units) where
  compileImpl (Squared' curve) =
    CompiledFunction.map Expression.squared' Qty.squared' Range.squared' (compiled curve)

  derivativeImpl _ (Squared' curve) =
    2.0 * curve .*. derivative curve

-- | Compute the square root of a curve.
sqrt :: Tolerance units1 => Units.Squared units1 units2 => Curve units2 -> Curve units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Tolerance units => Curve (units :*: units) -> Curve units
sqrt' curve =
  if Tolerance.using Tolerance.squared' (curve ~= Qty.zero)
    then zero
    else new (Sqrt' curve)

data Sqrt' units where
  Sqrt' :: Tolerance units => Curve (units :*: units) -> Sqrt' units

deriving instance Show (Sqrt' units)

-- TODO handle endpoint degeneracies
instance Interface (Sqrt' units) units where
  compileImpl (Sqrt' curve) =
    CompiledFunction.map Expression.sqrt' Qty.sqrt' Range.sqrt' (compiled curve)

  derivativeImpl self (Sqrt' curve) =
    if self ~= Qty.zero then zero else derivative curve .!/! (2.0 * self)

-- | Compute the sine of a curve.
sin :: Curve Radians -> Curve Unitless
sin = new . Sin

newtype Sin = Sin (Curve Radians) deriving (Show)

instance Interface Sin Unitless where
  compileImpl (Sin curve) =
    CompiledFunction.map Expression.sin Angle.sin Range.sin (compiled curve)

  derivativeImpl _ (Sin curve) =
    cos curve * (derivative curve / Angle.radian)

-- | Compute the cosine of a curve.
cos :: Curve Radians -> Curve Unitless
cos = new . Cos

newtype Cos = Cos (Curve Radians) deriving (Show)

instance Interface Cos Unitless where
  compileImpl (Cos curve) =
    CompiledFunction.map Expression.cos Angle.cos Range.cos (compiled curve)

  derivativeImpl _ (Cos curve) =
    negate (sin curve) * (derivative curve / Angle.radian)

integral :: Curve units -> Estimate units
integral curve = Estimate.new (Integral curve (derivative curve) Range.unit)

data Integral units = Integral (Curve units) (Curve units) (Range Unitless)

instance Estimate.Interface (Integral units) units where
  boundsImpl (Integral curve curveDerivative domain) = do
    let dx = Range.width domain
    let derivativeBounds = evaluateBounds curveDerivative domain
    let estimate0 = dx * evaluateBounds curve domain
    let y1 = evaluate curve (Range.lowerBound domain)
    let y2 = evaluate curve (Range.upperBound domain)
    let m = Range.width derivativeBounds
    let error1 = 0.125 * m * dx * dx
    let estimate1 = dx * Qty.midpoint y1 y2 + Range -error1 error1
    case Range.intersection estimate0 estimate1 of
      Just intersection -> intersection
      Nothing -> estimate0 -- Shouldn't happen if bounds are correct

  refineImpl (Integral curve curveDerivative domain) = do
    let (leftDomain, rightDomain) = Range.bisect domain
    let leftIntegral = Integral curve curveDerivative leftDomain
    let rightIntegral = Integral curve curveDerivative rightDomain
    Estimate.new leftIntegral + Estimate.new rightIntegral

----- ZERO FINDING -----

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
zeros :: Tolerance units => Curve units -> Result Zeros.Error (List Zero)
zeros curve
  | curve ~= Qty.zero = Failure Zeros.ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Stream.iterate derivative curve
      let derivativeBounds tRange = Stream.map (\f -> evaluateBounds f tRange) derivatives
      let cache = Solve1d.init derivativeBounds
      case Solve1d.search (findZeros derivatives) cache of
        Success foundZeros -> Success (List.sortBy Zero.location foundZeros)
        Failure Solve1d.InfiniteRecursion -> Failure Zeros.HigherOrderZero

findZeros ::
  Tolerance units =>
  Stream (Curve units) ->
  Domain1d ->
  Stream (Range units) ->
  Solve1d.Exclusions exclusions ->
  Solve1d.Action exclusions Zero
findZeros derivatives subdomain derivativeBounds exclusions
  -- Skip the subdomain entirely if the curve itself is non-zero everywhere
  | not (Stream.head derivativeBounds ^ Qty.zero) = Solve1d.pass
  -- Optimization heuristic: bisect down to small subdomains first,
  -- to quickly eliminate most of the curve based on simple value bounds
  -- before attempting more complex/sophisticated solving
  | Range.width (Domain1d.bounds subdomain) > 1 / 1024 = Solve1d.recurse
  | otherwise = case exclusions of
      Solve1d.SomeExclusions -> Solve1d.recurse
      Solve1d.NoExclusions ->
        case findZerosOrder 0 derivatives subdomain derivativeBounds of
          Unresolved -> Solve1d.recurse
          Resolved [] -> Solve1d.pass
          Resolved (NonEmpty subdomainZeros) -> do
            let subdomainInterior = Domain1d.interior subdomain
            if NonEmpty.allSatisfy (\(t0, _) -> Range.includes t0 subdomainInterior) subdomainZeros
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
  Stream (Range units) ->
  Fuzzy (List (Float, Solve1d.Neighborhood units))
findZerosOrder k derivatives subdomain derivativeBounds
  -- A derivative is resolved, so it has no zeros
  -- (note that if k == 0, we already checked for the curve being non-zero in findZeros above)
  | k > 0 && Range.isResolved (Stream.head derivativeBounds) = Resolved []
  -- We've exceeded the maximum zero order without finding a non-zero derivative
  | k > maxZeroOrder = Unresolved
  -- Otherwise, find higher-order zeros and then search in between them
  | otherwise = Fuzzy.do
      let higherDerivatives = Stream.tail derivatives
      let higherDerivativeBounds = Stream.tail derivativeBounds
      let currentDerivative = Stream.head derivatives
      let nextDerivative = Stream.head higherDerivatives
      let tRange = Domain1d.bounds subdomain
      higherOrderZeros <- findZerosOrder (k + 1) higherDerivatives subdomain higherDerivativeBounds
      case higherOrderZeros of
        [] -> solveMonotonic k currentDerivative nextDerivative tRange
        List.One (t0, neighborhood) -> do
          if Qty.abs (evaluate currentDerivative t0) <= Solve1d.derivativeTolerance neighborhood k
            then Resolved [(t0, neighborhood)]
            else Fuzzy.do
              let leftRange = Range (Range.lowerBound tRange) t0
              let rightRange = Range t0 (Range.upperBound tRange)
              leftZeros <- solveMonotonic k currentDerivative nextDerivative leftRange
              rightZeros <- solveMonotonic k currentDerivative nextDerivative rightRange
              Resolved (leftZeros + rightZeros)
        List.TwoOrMore -> Unresolved

solveMonotonic ::
  Tolerance units =>
  Int ->
  Curve units ->
  Curve units ->
  Range Unitless ->
  Fuzzy (List (Float, Solve1d.Neighborhood units))
solveMonotonic m fm fn tRange = do
  let n = m + 1
  let (tLow, tHigh) = Range.endpoints tRange
  let startNeighborhood = Solve1d.neighborhood n (evaluate fn tLow)
  if Qty.abs (evaluate fm tLow) <= Solve1d.derivativeTolerance startNeighborhood m
    then if tLow == 0.0 then Resolved [(0.0, startNeighborhood)] else Unresolved
    else do
      let endNeighborhood = Solve1d.neighborhood n (evaluate fn tHigh)
      if Qty.abs (evaluate fm tHigh) <= Solve1d.derivativeTolerance endNeighborhood m
        then if tHigh == 1.0 then Resolved [(1.0, endNeighborhood)] else Unresolved
        else do
          case Solve1d.monotonic (evaluate fm) (evaluate fn) tRange of
            Solve1d.Exact t0 -> Resolved [(t0, Solve1d.neighborhood n (evaluate fn t0))]
            Solve1d.Closest _ -> Unresolved
