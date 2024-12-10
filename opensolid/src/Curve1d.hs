module Curve1d
  ( Curve1d (Parametric)
  , Zero
  , Interface (..)
  , evaluate
  , evaluateBounds
  , derivative
  , new
  , zero
  , constant
  , t
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

import Angle qualified
import Composition
import Curve1d.Integral (Integral (Integral))
import Curve1d.Zero (Zero)
import Curve1d.Zero qualified as Zero
import Curve1d.Zeros qualified as Zeros
import Domain1d (Domain1d)
import Domain1d qualified
import Estimate (Estimate)
import Estimate qualified
import Expression (Expression)
import Expression qualified
import Fuzzy qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Parameter qualified
import Qty qualified
import Range (Range)
import Range qualified
import Solve1d qualified
import Stream (Stream)
import Stream qualified
import Units (Meters)
import Units qualified

class
  Show curve =>
  Interface curve units
    | curve -> units
  where
  evaluateImpl :: curve -> Float -> Qty units
  evaluateBoundsImpl :: curve -> Range Unitless -> Range units
  derivativeImpl :: curve -> Curve1d units

data Curve1d units where
  Curve1d ::
    Interface curve units =>
    curve ->
    Curve1d units
  Parametric ::
    Expression Float (Qty units) ->
    Curve1d units
  Negated ::
    Curve1d units ->
    Curve1d units
  Sum ::
    Curve1d units ->
    Curve1d units ->
    Curve1d units
  Difference ::
    Curve1d units ->
    Curve1d units ->
    Curve1d units
  Product' ::
    Curve1d units1 ->
    Curve1d units2 ->
    Curve1d (units1 :*: units2)
  Quotient' ::
    Curve1d units1 ->
    Curve1d units2 ->
    Curve1d (units1 :/: units2)
  Squared' ::
    Curve1d units ->
    Curve1d (units :*: units)
  SquareRoot' ::
    Curve1d (units :*: units) ->
    Curve1d units
  Sin ::
    Curve1d Radians ->
    Curve1d Unitless
  Cos ::
    Curve1d Radians ->
    Curve1d Unitless
  Coerce ::
    Curve1d units1 ->
    Curve1d units2
  Reversed ::
    Curve1d units ->
    Curve1d units

deriving instance Show (Curve1d units)

instance FFI (Curve1d Unitless) where
  representation = FFI.classRepresentation "Curve"

instance FFI (Curve1d Meters) where
  representation = FFI.classRepresentation "Length Curve"

instance FFI (Curve1d Radians) where
  representation = FFI.classRepresentation "Angle Curve"

instance HasUnits (Curve1d units) where
  type UnitsOf (Curve1d units) = units

instance Units.Coercion (Curve1d unitsA) (Curve1d unitsB) where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve1d units1) (Curve1d units2) units1
  where
  curve1 ~= curve2 = curve1 - curve2 ~= Qty.zero

instance
  units1 ~ units2 =>
  ApproximateEquality (Curve1d units1) (Qty units2) units1
  where
  curve ~= value = List.allTrue [evaluate curve tValue ~= value | tValue <- Parameter.samples]

instance
  units1 ~ units2 =>
  Intersects (Curve1d units1) (Qty units2) units1
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
  Intersects (Qty units1) (Curve1d units2) units1
  where
  value ^ curve = curve ^ value

instance Interface (Curve1d units) units where
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative

new :: Interface curve units => curve -> Curve1d units
new = Curve1d

zero :: Curve1d units
zero = constant Qty.zero

constant :: Qty units -> Curve1d units
constant = Parametric . Expression.constant

t :: Curve1d Unitless
t = Parametric Expression.t

instance Negation (Curve1d units) where
  negate (Parametric expression) = Parametric -expression
  negate (Negated curve) = curve
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product' c1 c2) = negate c1 .*. c2
  negate curve = Negated curve

instance Multiplication Sign (Curve1d units) (Curve1d units)

instance Multiplication' Sign (Curve1d units) where
  type Sign .*. Curve1d units = Curve1d (Unitless :*: units)
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Multiplication (Curve1d units) Sign (Curve1d units)

instance Multiplication' (Curve1d units) Sign where
  type Curve1d units .*. Sign = Curve1d (units :*: Unitless)
  curve .*. Positive = Units.coerce curve
  curve .*. Negative = Units.coerce -curve

instance units ~ units_ => Addition (Curve1d units) (Curve1d units_) (Curve1d units) where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

instance units ~ units_ => Addition (Curve1d units) (Qty units_) (Curve1d units) where
  curve + value = curve + constant value

instance units ~ units_ => Addition (Qty units) (Curve1d units_) (Curve1d units) where
  value + curve = constant value + curve

instance
  units1 ~ units2 =>
  Subtraction (Curve1d units1) (Curve1d units2) (Curve1d units1)
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance
  units1 ~ units2 =>
  Subtraction (Curve1d units1) (Qty units2) (Curve1d units1)
  where
  curve - value = curve - constant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Curve1d units2) (Curve1d units1)
  where
  value - curve = constant value - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Multiplication' (Curve1d units1) (Curve1d units2) where
  type Curve1d units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Qty units2) (Curve1d units3)

instance Multiplication' (Curve1d units1) (Qty units2) where
  type Curve1d units1 .*. Qty units2 = Curve1d (units1 :*: units2)
  curve .*. value = curve .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Curve1d units2) (Curve1d units3)

instance Multiplication' (Qty units1) (Curve1d units2) where
  type Qty units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  value .*. curve = constant value .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Division' (Curve1d units1) (Curve1d units2) where
  type Curve1d units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  Parametric lhs ./. Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1d units1) (Qty units2) (Curve1d units3)

instance Division' (Curve1d units1) (Qty units2) where
  type Curve1d units1 ./. Qty units2 = Curve1d (units1 :/: units2)
  curve ./. value = curve ./. constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Curve1d units2) (Curve1d units3)

instance Division' (Qty units1) (Curve1d units2) where
  type Qty units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  value ./. curve = constant value ./. curve

instance Composition (Curve1d Unitless) (Curve1d units) (Curve1d units) where
  Parametric outer . Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance Interface (Curve1d units :.: Curve1d Unitless) units where
  evaluateImpl (outer :.: inner) tValue =
    evaluate outer (evaluate inner tValue)
  evaluateBoundsImpl (outer :.: inner) tRange =
    evaluateBounds outer (evaluateBounds inner tRange)
  derivativeImpl (outer :.: inner) =
    (derivative outer . inner) * derivative inner

evaluate :: Curve1d units -> Float -> Qty units
evaluate curve tValue = case curve of
  Curve1d c -> evaluateImpl c tValue
  Parametric expression -> Expression.evaluate expression tValue
  Negated c -> negate (evaluate c tValue)
  Sum c1 c2 -> evaluate c1 tValue + evaluate c2 tValue
  Difference c1 c2 -> evaluate c1 tValue - evaluate c2 tValue
  Product' c1 c2 -> evaluate c1 tValue .*. evaluate c2 tValue
  Quotient' c1 c2 -> evaluate c1 tValue ./. evaluate c2 tValue
  Squared' c -> Qty.squared' (evaluate c tValue)
  SquareRoot' c' -> Qty.sqrt' (evaluate c' tValue)
  Sin c -> Angle.sin (evaluate c tValue)
  Cos c -> Angle.cos (evaluate c tValue)
  Coerce c -> Units.coerce (evaluate c tValue)
  Reversed c -> evaluate c (1.0 - tValue)

evaluateBounds :: Curve1d units -> Range Unitless -> Range units
evaluateBounds curve tRange = case curve of
  Curve1d c -> evaluateBoundsImpl c tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Negated c -> negate (evaluateBounds c tRange)
  Sum c1 c2 -> evaluateBounds c1 tRange + evaluateBounds c2 tRange
  Difference c1 c2 -> evaluateBounds c1 tRange - evaluateBounds c2 tRange
  Product' c1 c2 -> evaluateBounds c1 tRange .*. evaluateBounds c2 tRange
  Quotient' c1 c2 -> evaluateBounds c1 tRange ./. evaluateBounds c2 tRange
  Squared' c -> Range.squared' (evaluateBounds c tRange)
  SquareRoot' c' -> Range.sqrt' (evaluateBounds c' tRange)
  Sin c -> Range.sin (evaluateBounds c tRange)
  Cos c -> Range.cos (evaluateBounds c tRange)
  Coerce c -> Units.coerce (evaluateBounds c tRange)
  Reversed c -> evaluateBounds c (1.0 - tRange)

derivative :: Curve1d units -> Curve1d units
derivative curve = case curve of
  Curve1d c -> derivativeImpl c
  Parametric expression -> Parametric (Expression.curveDerivative expression)
  Negated c -> negate (derivative c)
  Sum c1 c2 -> derivative c1 + derivative c2
  Difference c1 c2 -> derivative c1 - derivative c2
  Product' c1 c2 -> derivative c1 .*. c2 + c1 .*. derivative c2
  Quotient' c1 c2 -> (derivative c1 .*. c2 - c1 .*. derivative c2) .!/.! squared' c2
  Squared' c -> 2.0 * c .*. derivative c
  SquareRoot' c' -> derivative c' .!/! (2.0 * sqrt' c')
  Sin c -> cos c * (derivative c / Angle.radian)
  Cos c -> negate (sin c) * (derivative c / Angle.radian)
  Coerce c -> Units.coerce (derivative c)
  Reversed c -> -(reverse (derivative c))

reverse :: Curve1d units -> Curve1d units
reverse (Parametric expression) = Parametric (expression . Expression.r)
reverse curve = Curve1d (Reversed curve)

squared :: Units.Squared units1 units2 => Curve1d units1 -> Curve1d units2
squared curve = Units.specialize (squared' curve)

squared' :: Curve1d units -> Curve1d (units :*: units)
squared' (Parametric expression) = Parametric (Expression.squared' expression)
squared' (Negated c) = squared' c
squared' curve = Squared' curve

sqrt :: Units.Squared units1 units2 => Curve1d units2 -> Curve1d units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Curve1d (units :*: units) -> Curve1d units
sqrt' (Parametric expression) = Parametric (Expression.sqrt' expression)
sqrt' curve = SquareRoot' curve

sin :: Curve1d Radians -> Curve1d Unitless
sin (Parametric expression) = Parametric (Expression.sin expression)
sin curve = Sin curve

cos :: Curve1d Radians -> Curve1d Unitless
cos (Parametric expression) = Parametric (Expression.cos expression)
cos curve = Cos curve

integral :: Curve1d units -> Estimate units
integral curve = Estimate.new (Integral curve (derivative curve) Range.unit)

----- ZERO FINDING -----

zeros :: Tolerance units => Curve1d units -> Result Zeros.Error (List Zero)
zeros curve
  | curve ~= Qty.zero = Failure Zeros.ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Stream.iterate curve derivative
      let derivativeBounds tRange = Stream.map (\f -> evaluateBounds f tRange) derivatives
      let cache = Solve1d.init derivativeBounds
      case Solve1d.search (findZeros derivatives) cache of
        Success foundZeros -> Success (List.sortBy Zero.location foundZeros)
        Failure Solve1d.InfiniteRecursion -> Failure Zeros.HigherOrderZero

findZeros ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Domain1d ->
  Stream (Range units) ->
  Solve1d.Exclusions exclusions ->
  Solve1d.Action exclusions Zero
findZeros derivatives subdomain derivativeBounds exclusions
  -- Skip the subdomain entirely if the curve itself is non-zero everywhere
  | not (Stream.head derivativeBounds ^ Qty.zero) = Solve1d.pass
  -- Optimization heuristic: bisect down to "smallish" domains first,
  -- to quickly eliminate most of the curve based on simple value bounds
  -- before attempting more complex/sophisticated solving
  | Range.width (Domain1d.bounds subdomain) > 1 / 16 = Solve1d.recurse
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
  Stream (Curve1d units) ->
  Domain1d ->
  Stream (Range units) ->
  Fuzzy (List (Float, Solve1d.Neighborhood units))
findZerosOrder k derivatives subdomain derivativeBounds
  -- The function itself is non-zero, so no zeros
  | k == 0 && not (Stream.head derivativeBounds ^ Qty.zero) = Resolved []
  -- A derivative is resolved, so it has no zeros
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
              let leftRange = Range.from (Range.lowerBound tRange) t0
              let rightRange = Range.from t0 (Range.upperBound tRange)
              leftZeros <- solveMonotonic k currentDerivative nextDerivative leftRange
              rightZeros <- solveMonotonic k currentDerivative nextDerivative rightRange
              Resolved (leftZeros + rightZeros)
        List.TwoOrMore{} -> Unresolved

solveMonotonic ::
  Tolerance units =>
  Int ->
  Curve1d units ->
  Curve1d units ->
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
          let t0 = Solve1d.monotonic (evaluate fm) (evaluate fn) tRange
          if t0 == tLow || t0 == tHigh
            then Unresolved
            else Resolved [(t0, Solve1d.neighborhood n (evaluate fn t0))]
