module Curve1d
  ( Curve1d
  , Interface (..)
  , pointOn
  , segmentBounds
  , derivative
  , new
  , zero
  , constant
  , parameter
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  , zeros
  , reverse
  , integral
  , toAst
  )
where

import Angle qualified
import Curve1d.Integral (Integral (Integral))
import Curve1d.Root (Root)
import Curve1d.Root qualified as Root
import Curve1d.Zeros qualified as Zeros
import Domain1d (Domain1d)
import Domain1d qualified
import Estimate (Estimate)
import Estimate qualified
import Float qualified
import Fuzzy qualified
import Jit qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Parameter qualified
import Qty qualified
import Range (Range)
import Range qualified
import Solve1d qualified
import Stream (Stream)
import Stream qualified
import Typeable qualified
import Units qualified

class Known curve => Interface curve units | curve -> units where
  pointOnImpl :: curve -> Float -> Qty units
  segmentBoundsImpl :: curve -> Range Unitless -> Range units
  derivativeImpl :: curve -> Curve1d units
  toAstImpl :: curve -> Jit.Ast Float Float

data Curve1d units where
  Curve1d ::
    Interface curve units =>
    curve ->
    Curve1d units
  Constant ::
    Qty units -> Curve1d units
  Parameter ::
    Curve1d Unitless
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
    (Known units1, Known units2) =>
    Curve1d units1 ->
    Curve1d units2 ->
    Curve1d (units1 :*: units2)
  Quotient' ::
    (Known units1, Known units2) =>
    Curve1d units1 ->
    Curve1d units2 ->
    Curve1d (units1 :/: units2)
  Squared' ::
    Known units =>
    Curve1d units ->
    Curve1d (units :*: units)
  SquareRoot' ::
    Known (units :*: units) =>
    Curve1d (units :*: units) ->
    Curve1d units
  Sin ::
    Curve1d Radians ->
    Curve1d Unitless
  Cos ::
    Curve1d Radians ->
    Curve1d Unitless
  Coerce ::
    (Known units1, Known units2) =>
    Curve1d units1 ->
    Curve1d units2

instance Eq (Curve1d units) where
  Curve1d c1 == Curve1d c2 = Typeable.equal c1 c2
  Curve1d{} == _ = False
  Constant x == Constant y = x == y
  Constant{} == _ = False
  Parameter == Parameter = True
  Parameter{} == _ = False
  Negated c1 == Negated c2 = c1 == c2
  Negated{} == _ = False
  Sum lhs1 rhs1 == Sum lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Sum{} == _ = False
  Difference lhs1 rhs1 == Difference lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Difference{} == _ = False
  Product' lhs1 rhs1 == Product' lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Product'{} == _ = False
  Quotient' lhs1 rhs1 == Quotient' lhs2 rhs2 = lhs1 == lhs2 && rhs1 == rhs2
  Quotient'{} == _ = False
  Squared' c1 == Squared' c2 = c1 == c2
  Squared'{} == _ = False
  SquareRoot' c1 == SquareRoot' c2 = c1 == c2
  SquareRoot'{} == _ = False
  Sin c1 == Sin c2 = c1 == c2
  Sin{} == _ = False
  Cos c1 == Cos c2 = c1 == c2
  Cos{} == _ = False
  Coerce c1 == Coerce c2 = Typeable.equal c1 c2
  Coerce{} == _ = False

deriving instance Show (Curve1d units)

instance HasUnits (Curve1d units) where
  type UnitsOf (Curve1d units) = units

instance (Known unitsA, Known unitsB) => Units.Coercion (Curve1d unitsA) (Curve1d unitsB) where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance
  (Known units1, Known units2, units1 ~ units2) =>
  ApproximateEquality (Curve1d units1) (Curve1d units2) units1
  where
  curve1 ~= curve2 = curve1 - curve2 ~= Qty.zero

instance units ~ units_ => ApproximateEquality (Curve1d units) (Qty units_) units where
  Constant value1 ~= value2 = value1 ~= value2
  curve ~= value = List.allTrue [pointOn curve tValue ~= value | tValue <- Parameter.samples]

instance
  (Known units1, Known units2, units1 ~ units2) =>
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
  (Known units1, Known units2, units1 ~ units2) =>
  Intersects (Qty units1) (Curve1d units2) units1
  where
  value ^ curve = curve ^ value

instance Known units => Interface (Curve1d units) units where
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  toAstImpl = toAst

new :: Interface curve units => curve -> Curve1d units
new = Curve1d

zero :: Curve1d units
zero = constant Qty.zero

constant :: Qty units -> Curve1d units
constant = Constant

parameter :: Curve1d Unitless
parameter = Parameter

instance Known units => Negation (Curve1d units) where
  negate (Constant x) = Constant (negate x)
  negate (Negated curve) = curve
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product' c1 c2) = negate c1 .*. c2
  negate curve = Negated curve

instance Known units => Multiplication Sign (Curve1d units) (Curve1d units)

instance Known units => Multiplication' Sign (Curve1d units) where
  type Sign .*. Curve1d units = Curve1d (Unitless :*: units)
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Known units => Multiplication (Curve1d units) Sign (Curve1d units)

instance Known units => Multiplication' (Curve1d units) Sign where
  type Curve1d units .*. Sign = Curve1d (units :*: Unitless)
  curve .*. Positive = Units.coerce curve
  curve .*. Negative = Units.coerce -curve

instance units ~ units_ => Addition (Curve1d units) (Curve1d units_) (Curve1d units) where
  curve + Constant x | x == Qty.zero = curve
  Constant x + curve | x == Qty.zero = curve
  Constant x + Constant y = constant (x + y)
  curve1 + curve2 = Sum curve1 curve2

instance units ~ units_ => Addition (Curve1d units) (Qty units_) (Curve1d units) where
  curve + value = curve + constant value

instance units ~ units_ => Addition (Qty units) (Curve1d units_) (Curve1d units) where
  value + curve = constant value + curve

instance Addition (Curve1d Unitless) Int (Curve1d Unitless) where
  curve + value = curve + Float.int value

instance Addition Int (Curve1d Unitless) (Curve1d Unitless) where
  value + curve = Float.int value + curve

instance Subtraction (Curve1d Unitless) Int (Curve1d Unitless) where
  curve - value = curve - Float.int value

instance Subtraction Int (Curve1d Unitless) (Curve1d Unitless) where
  value - curve = Float.int value - curve

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Curve1d units1) (Curve1d units2) (Curve1d units1)
  where
  curve - Constant x | x == Qty.zero = curve
  Constant x - curve | x == Qty.zero = negate curve
  Constant x - Constant y = constant (x - y)
  curve1 - curve2 = Difference curve1 curve2

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Curve1d units1) (Qty units2) (Curve1d units1)
  where
  curve - value = curve - constant value

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Qty units1) (Curve1d units2) (Curve1d units1)
  where
  value - curve = constant value - curve

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Curve1d units1) (Curve1d units2)
  where
  type Curve1d units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  Constant x .*. _ | x == Qty.zero = zero
  _ .*. Constant x | x == Qty.zero = zero
  Constant x .*. Constant y = Constant (x .*. y)
  Constant x .*. curve | x == Units.coerce 1.0 = Units.coerce curve
  Constant x .*. curve | x == Units.coerce -1.0 = Units.coerce (negate curve)
  Constant x .*. Negated c = negate x .*. c
  c1 .*. (Constant x) = Units.commute (Constant x .*. c1)
  Constant x .*. Product' (Constant y) c = Units.rightAssociate ((x .*. y) .*. c)
  curve1 .*. curve2 = Product' curve1 curve2

instance
  Known units =>
  Multiplication' Int (Curve1d units)
  where
  type Int .*. Curve1d units = Curve1d (Unitless :*: units)
  value .*. curve = Float.int value .*. curve

instance
  Known units =>
  Multiplication' (Curve1d units) Int
  where
  type Curve1d units .*. Int = Curve1d (units :*: Unitless)
  curve .*. value = curve .*. Float.int value

instance Known units => Multiplication Int (Curve1d units) (Curve1d units)

instance Known units => Multiplication (Curve1d units) Int (Curve1d units)

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Curve1d units1) (Qty units2) (Curve1d units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Curve1d units1) (Qty units2)
  where
  type Curve1d units1 .*. Qty units2 = Curve1d (units1 :*: units2)
  curve .*. value = curve .*. constant value

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Qty units1) (Curve1d units2) (Curve1d units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Qty units1) (Curve1d units2)
  where
  type Qty units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  value .*. curve = constant value .*. curve

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance
  (Known units1, Known units2) =>
  Division' (Curve1d units1) (Curve1d units2)
  where
  type Curve1d units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  Constant x ./. _ | x == Qty.zero = zero
  Constant x ./. Constant y = Constant (x ./. y)
  curve ./. Constant x = (1 ./. x) .*^ curve
  curve1 ./. curve2 = Quotient' curve1 curve2

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Curve1d units1) (Qty units2) (Curve1d units3)

instance
  (Known units1, Known units2) =>
  Division' (Curve1d units1) (Qty units2)
  where
  type Curve1d units1 ./. Qty units2 = Curve1d (units1 :/: units2)
  curve ./. value = curve ./. constant value

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Qty units1) (Curve1d units2) (Curve1d units3)

instance
  (Known units1, Known units2) =>
  Division' (Qty units1) (Curve1d units2)
  where
  type Qty units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  value ./. curve = constant value ./. curve

instance Known units => Division (Curve1d units) Int (Curve1d units)

instance Known units => Division' (Curve1d units) Int where
  type Curve1d units ./. Int = Curve1d (units :/: Unitless)
  curve ./. value = curve ./. Float.int value

instance
  (Known units1, Known units2, Units.Inverse units1 units2) =>
  Division Int (Curve1d units1) (Curve1d units2)

instance Known units => Division' Int (Curve1d units) where
  type Int ./. Curve1d units = Curve1d (Unitless :/: units)
  value ./. curve = Float.int value ./. curve

pointOn :: Curve1d units -> Float -> Qty units
pointOn curve tValue = case curve of
  Curve1d c -> pointOnImpl c tValue
  Constant x -> x
  Parameter -> tValue
  Negated c -> negate (pointOn c tValue)
  Sum c1 c2 -> pointOn c1 tValue + pointOn c2 tValue
  Difference c1 c2 -> pointOn c1 tValue - pointOn c2 tValue
  Product' c1 c2 -> pointOn c1 tValue .*. pointOn c2 tValue
  Quotient' c1 c2 -> pointOn c1 tValue ./. pointOn c2 tValue
  Squared' c -> Qty.squared' (pointOn c tValue)
  SquareRoot' c' -> Qty.sqrt' (pointOn c' tValue)
  Sin c -> Angle.sin (pointOn c tValue)
  Cos c -> Angle.cos (pointOn c tValue)
  Coerce c -> Units.coerce (pointOn c tValue)

segmentBounds :: Curve1d units -> Range Unitless -> Range units
segmentBounds curve tBounds = case curve of
  Curve1d c -> segmentBoundsImpl c tBounds
  Constant value -> Range.constant value
  Parameter -> tBounds
  Negated c -> negate (segmentBounds c tBounds)
  Sum c1 c2 -> segmentBounds c1 tBounds + segmentBounds c2 tBounds
  Difference c1 c2 -> segmentBounds c1 tBounds - segmentBounds c2 tBounds
  Product' c1 c2 -> segmentBounds c1 tBounds .*. segmentBounds c2 tBounds
  Quotient' c1 c2 -> segmentBounds c1 tBounds ./. segmentBounds c2 tBounds
  Squared' c -> Range.squared' (segmentBounds c tBounds)
  SquareRoot' c' -> Range.sqrt' (segmentBounds c' tBounds)
  Sin c -> Range.sin (segmentBounds c tBounds)
  Cos c -> Range.cos (segmentBounds c tBounds)
  Coerce c -> Units.coerce (segmentBounds c tBounds)

derivative :: Known units => Curve1d units -> Curve1d units
derivative curve = case curve of
  Curve1d c -> derivativeImpl c
  Constant _ -> zero
  Parameter -> constant 1.0
  Negated c -> negate (derivative c)
  Sum c1 c2 -> derivative c1 + derivative c2
  Difference c1 c2 -> derivative c1 - derivative c2
  Product' c1 c2 -> derivative c1 .*. c2 + c1 .*. derivative c2
  Quotient' c1 c2 -> (derivative c1 .*. c2 - c1 .*. derivative c2) .!/.! squared' c2
  Squared' c -> 2 * c .*. derivative c
  SquareRoot' c' -> derivative c' .!/! (2 * sqrt' c')
  Sin c -> cos c * (derivative c / Angle.radian)
  Cos c -> negate (sin c) * (derivative c / Angle.radian)
  Coerce c -> Units.coerce (derivative c)

newtype Reversed units = Reversed (Curve1d units) deriving (Eq, Show)

instance Known units => Interface (Reversed units) units where
  pointOnImpl (Reversed curve) tValue = pointOn curve (1 - tValue)
  segmentBoundsImpl (Reversed curve) tBounds = segmentBounds curve (1 - tBounds)
  derivativeImpl (Reversed curve) = -(reverse (derivative curve))
  toAstImpl (Reversed curve) = toAst curve . toAst (1.0 - parameter)

reverse :: Known units => Curve1d units -> Curve1d units
reverse curve@(Constant _) = curve
reverse curve = Curve1d (Reversed curve)

squared ::
  (Known units1, Known units2, Units.Squared units1 units2) =>
  Curve1d units1 ->
  Curve1d units2
squared curve = Units.specialize (squared' curve)

squared' :: Known units => Curve1d units -> Curve1d (units :*: units)
squared' curve = case curve of
  Constant x -> Constant (x .*. x)
  Negated c -> squared' c
  Cos c -> Units.unspecialize (cosSquared c)
  Sin c -> Units.unspecialize (sinSquared c)
  _ -> Squared' curve

cosSquared :: Curve1d Radians -> Curve1d Unitless
cosSquared c = 0.5 * cos (2 * c) + 0.5

sinSquared :: Curve1d Radians -> Curve1d Unitless
sinSquared c = 0.5 - 0.5 * cos (2 * c)

sqrt ::
  (Known units1, Known units2, Units.Squared units1 units2) =>
  Curve1d units2 ->
  Curve1d units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Known units => Curve1d (units :*: units) -> Curve1d units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' curve = SquareRoot' curve

sin :: Curve1d Radians -> Curve1d Unitless
sin (Constant x) = constant (Angle.sin x)
sin curve = Sin curve

cos :: Curve1d Radians -> Curve1d Unitless
cos (Constant x) = constant (Angle.cos x)
cos curve = Cos curve

integral :: Known units => Curve1d units -> Estimate units
integral curve = Estimate.new (Integral curve (derivative curve) Range.unit)

toAst :: Curve1d units -> Jit.Ast Float Float
toAst curve = case curve of
  Curve1d c -> toAstImpl c
  Constant x -> Jit.constant (Units.coerce x)
  Parameter -> Jit.input
  Negated c -> Jit.negate (toAst c)
  Sum c1 c2 -> Jit.sum (toAst c1) (toAst c2)
  Difference c1 c2 -> Jit.sum (toAst c1) (toAst c2)
  Product' c1 c2 -> Jit.product (toAst c1) (toAst c2)
  Quotient' c1 c2 -> Jit.quotient (toAst c1) (toAst c2)
  Squared' c -> Jit.squared (toAst c)
  SquareRoot' c -> Jit.sqrt (toAst c)
  Sin c -> Jit.sin (toAst c)
  Cos c -> Jit.cos (toAst c)
  Coerce c -> toAst c

----- ROOT FINDING -----

zeros :: (Known units, Tolerance units) => Curve1d units -> Result Zeros.Error (List Root)
zeros curve
  | curve ~= Qty.zero = Failure Zeros.ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Stream.iterate curve derivative
      let derivativeBounds tBounds = Stream.map (\f -> segmentBounds f tBounds) derivatives
      let cache = Solve1d.init derivativeBounds
      case Solve1d.search (findZeros derivatives) cache of
        Success roots -> Success (List.sortBy Root.value roots)
        Failure Solve1d.InfiniteRecursion -> Failure Zeros.HigherOrderZero

findZeros ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Domain1d ->
  Stream (Range units) ->
  Solve1d.Exclusions exclusions ->
  Solve1d.Action exclusions Root
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
              then Solve1d.return (NonEmpty.map toRoot subdomainZeros)
              else Solve1d.recurse

toRoot :: (Float, Solve1d.Neighborhood units) -> Root
toRoot (t0, neighborhood) = Solve1d.root t0 neighborhood

maxRootOrder :: Int
maxRootOrder = 3

findZerosOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve1d units) ->
  Domain1d ->
  Stream (Range units) ->
  Fuzzy (List (Float, Solve1d.Neighborhood units))
findZerosOrder k derivatives subdomain derivativeBounds
  -- The function itself is non-zero, so no roots
  | k == 0 && not (Stream.head derivativeBounds ^ Qty.zero) = Resolved []
  -- A derivative is resolved, so it has no zeros
  | k > 0 && Range.isResolved (Stream.head derivativeBounds) = Resolved []
  -- We've exceeded the maximum root order without finding a non-zero derivative
  | k > maxRootOrder = Unresolved
  -- Otherwise, find higher-order roots and then search in between them
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
          if Qty.abs (pointOn currentDerivative t0) <= Solve1d.derivativeTolerance neighborhood k
            then Resolved [(t0, neighborhood)]
            else Fuzzy.do
              let leftRange = Range.from (Range.minValue tRange) t0
              let rightRange = Range.from t0 (Range.maxValue tRange)
              leftZeros <- solveMonotonic k currentDerivative nextDerivative leftRange
              rightZeros <- solveMonotonic k currentDerivative nextDerivative rightRange
              Resolved (leftZeros + rightZeros)
        List.TwoOrMore -> Unresolved

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
  let startNeighborhood = Solve1d.neighborhood n (pointOn fn tLow)
  if Qty.abs (pointOn fm tLow) <= Solve1d.derivativeTolerance startNeighborhood m
    then if tLow == 0.0 then Resolved [(0.0, startNeighborhood)] else Unresolved
    else do
      let endNeighborhood = Solve1d.neighborhood n (pointOn fn tHigh)
      if Qty.abs (pointOn fm tHigh) <= Solve1d.derivativeTolerance endNeighborhood m
        then if tHigh == 1.0 then Resolved [(1.0, endNeighborhood)] else Unresolved
        else do
          let t0 = Solve1d.monotonic (pointOn fm) (pointOn fn) tRange
          if t0 == tLow || t0 == tHigh
            then Unresolved
            else Resolved [(t0, Solve1d.neighborhood n (pointOn fn t0))]
