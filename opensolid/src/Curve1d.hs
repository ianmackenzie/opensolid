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
  , isZero
  , hasZero
  , zeros
  , reverse
  , integral
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
import List qualified
import OpenSolid
import Parameter qualified
import Qty qualified
import Radians qualified
import Range (Range)
import Range qualified
import Solve1d qualified
import Stream (Stream)
import Stream qualified
import Units qualified

class Show curve => Interface curve units | curve -> units where
  pointOnImpl :: curve -> Float -> Qty units
  segmentBoundsImpl :: curve -> Range Unitless -> Range units
  derivativeImpl :: curve -> Curve1d units

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

deriving instance Show (Curve1d units)

instance HasUnits (Curve1d units) where
  type Units (Curve1d units) = units
  type Erase (Curve1d units) = Curve1d Unitless

instance Units.Coercion (Curve1d unitsA) (Curve1d unitsB) where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance units ~ units_ => ApproximateEquality (Curve1d units) (Curve1d units_) units where
  curve1 ~= curve2 = isZero (curve1 - curve2)

instance units ~ units_ => ApproximateEquality (Curve1d units) (Qty units_) units where
  curve ~= value = isZero (curve - value)

instance Interface (Curve1d units) units where
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative

new :: Interface curve units => curve -> Curve1d units
new = Curve1d

zero :: Curve1d units
zero = constant Qty.zero

constant :: Qty units -> Curve1d units
constant = Constant

parameter :: Curve1d Unitless
parameter = Parameter

instance Negation (Curve1d units) where
  negate (Constant x) = Constant (negate x)
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
  curve + Constant (Qty 0.0) = curve
  Constant (Qty 0.0) + curve = curve
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

instance units ~ units_ => Subtraction (Curve1d units) (Curve1d units_) (Curve1d units) where
  curve - Constant (Qty 0.0) = curve
  Constant (Qty 0.0) - curve = negate curve
  Constant x - Constant y = constant (x - y)
  curve1 - curve2 = Difference curve1 curve2

instance units ~ units_ => Subtraction (Curve1d units) (Qty units_) (Curve1d units) where
  curve - value = curve - constant value

instance units ~ units_ => Subtraction (Qty units) (Curve1d units_) (Curve1d units) where
  value - curve = constant value - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Multiplication' (Curve1d units1) (Curve1d units2) where
  type Curve1d units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  Constant (Qty 0.0) .*. _ = zero
  _ .*. Constant (Qty 0.0) = zero
  Constant x .*. Constant y = Constant (x .*. y)
  Constant (Qty 1.0) .*. curve = Units.coerce curve
  Constant (Qty -1.0) .*. curve = Units.coerce (negate curve)
  Constant x .*. Negated c = negate x .*. c
  c1 .*. (Constant x) = Units.commute (Constant x .*. c1)
  Constant x .*. Product' (Constant y) c = Units.rightAssociate ((x .*. y) .*. c)
  curve1 .*. curve2 = Product' curve1 curve2

instance Multiplication' Int (Curve1d units) where
  type Int .*. Curve1d units = Curve1d (Unitless :*: units)
  value .*. curve = Float.int value .*. curve

instance Multiplication' (Curve1d units) Int where
  type Curve1d units .*. Int = Curve1d (units :*: Unitless)
  curve .*. value = curve .*. Float.int value

instance Multiplication Int (Curve1d units) (Curve1d units)

instance Multiplication (Curve1d units) Int (Curve1d units)

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
  Constant (Qty 0.0) ./. _ = zero
  Constant x ./. Constant y = Constant (x ./. y)
  curve ./. Constant x = (1 ./. x) .*^ curve
  curve1 ./. curve2 = Quotient' curve1 curve2

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

instance Division (Curve1d units) Int (Curve1d units)

instance Division' (Curve1d units) Int where
  type Curve1d units ./. Int = Curve1d (units :/: Unitless)
  curve ./. value = curve ./. Float.int value

instance
  Units.Quotient Unitless units1 units2 =>
  Division Int (Curve1d units1) (Curve1d units2)

instance Division' Int (Curve1d units) where
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

derivative :: Curve1d units -> Curve1d units
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
  Sin c -> cos c * Radians.toUnitless (derivative c)
  Cos c -> negate (sin c) * Radians.toUnitless (derivative c)
  Coerce c -> Units.coerce (derivative c)

newtype Reversed units = Reversed (Curve1d units)

deriving instance Show (Reversed units)

instance Interface (Reversed units) units where
  pointOnImpl (Reversed curve) tValue = pointOn curve (1 - tValue)
  segmentBoundsImpl (Reversed curve) tBounds = segmentBounds curve (1 - tBounds)
  derivativeImpl (Reversed curve) = -(reverse (derivative curve))

reverse :: Curve1d units -> Curve1d units
reverse curve@(Constant _) = curve
reverse curve = Curve1d (Reversed curve)

squared :: Units.Squared units1 units2 => Curve1d units1 -> Curve1d units2
squared curve = Units.specialize (squared' curve)

squared' :: Curve1d units -> Curve1d (units :*: units)
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

sqrt :: Units.Squared units1 units2 => Curve1d units2 -> Curve1d units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Curve1d (units :*: units) -> Curve1d units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' curve = SquareRoot' curve

sin :: Curve1d Radians -> Curve1d Unitless
sin (Constant x) = constant (Angle.sin x)
sin curve = Sin curve

cos :: Curve1d Radians -> Curve1d Unitless
cos (Constant x) = constant (Angle.cos x)
cos curve = Cos curve

isZero :: Tolerance units => Curve1d units -> Bool
isZero (Constant value) = value ~= Qty.zero
isZero curve = List.allTrue [pointOn curve tValue ~= Qty.zero | tValue <- Parameter.samples]

hasZero :: Tolerance units => Curve1d units -> Bool
hasZero curve =
  -- TODO optimize this to use a special Solve1d.find or similar
  -- to efficiently check if there is *a* zero anywhere
  -- instead of finding *all* zeros (and their exact locations)
  case zeros curve of
    Success [] -> False
    Success List.OneOrMore -> True
    Failure Zeros.ZeroEverywhere -> True
    Failure Zeros.HigherOrderZero -> True

----- ROOT FINDING -----

zeros :: Tolerance units => Curve1d units -> Result Zeros.Error (List Root)
zeros curve
  | isZero curve = Failure Zeros.ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Stream.iterate curve derivative
      let derivativeBounds tBounds = Stream.map (\f -> segmentBounds f tBounds) derivatives
      let cache = Solve1d.init derivativeBounds
      case Solve1d.search (findZeros derivatives) cache [] [] of
        Success (roots, _) -> Success (List.sortBy Root.value roots)
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
  -- before attempting more sophisticated/complex solving
  | Range.width (Domain1d.bounds subdomain) > 1 / 16 = Solve1d.recurse
  | otherwise = case exclusions of
      Solve1d.SomeExclusions _ -> Solve1d.recurse
      Solve1d.NoExclusions -> findMonotonicOrder derivatives subdomain derivativeBounds 0

maxRootOrder :: Int
maxRootOrder = 3

findMonotonicOrder ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Domain1d ->
  Stream (Range units) ->
  Int ->
  Solve1d.Action Solve1d.NoExclusions Root
findMonotonicOrder derivatives subdomain derivativeBounds candidateOrder
  | candidateOrder > maxRootOrder = Solve1d.recurse
  | Range.isResolved (Stream.nth (candidateOrder + 1) derivativeBounds) = do
      let subdomainRoots = solveMonotonic derivatives candidateOrder (Domain1d.bounds subdomain)
      -- Only actually report those roots if they're all within the interior of the subdomain;
      -- otherwise recurse to try a different (offset or smaller) subdomain
      let subdomainInterior = Domain1d.interior subdomain
      let isInterior root = Range.includes (Root.value root) subdomainInterior
      if List.allSatisfy isInterior subdomainRoots
        then Solve1d.return subdomainRoots
        else Solve1d.recurse
  | otherwise = findMonotonicOrder derivatives subdomain derivativeBounds (candidateOrder + 1)

solveMonotonic ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Int ->
  Range Unitless ->
  List Root
solveMonotonic derivatives m tRange = do
  let n = m + 1
  let fm = Stream.nth m derivatives
  let fn = Stream.nth n derivatives
  if
    | Range.includes 0.0 tRange
    , let neighborhood = Solve1d.neighborhood n (pointOn fn 0.0)
    , Qty.abs (pointOn fm 0.0) <= Solve1d.derivativeTolerance neighborhood m ->
        bifurcate derivatives tRange 0.0 n neighborhood m
    | Range.includes 1.0 tRange
    , let neighborhood = Solve1d.neighborhood n (pointOn fn 1.0)
    , Qty.abs (pointOn fm 1.0) <= Solve1d.derivativeTolerance neighborhood m ->
        bifurcate derivatives tRange 1.0 n neighborhood m
    | otherwise -> solveMonotonicInterior derivatives tRange m

solveMonotonicInterior ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Range Unitless ->
  Int ->
  List Root
solveMonotonicInterior derivatives tRange m = do
  let n = m + 1
  let fm = Stream.nth m derivatives
  let fn = Stream.nth n derivatives
  let t0 = Solve1d.monotonic (pointOn fm) (pointOn fn) tRange
  if t0 == Range.minValue tRange || t0 == Range.maxValue tRange
    then do
      -- No actual zero of the mth derivative in this subdomain;
      -- if m = 0 then that means there are no roots in this subdomain,
      -- otherwise it means that the (m - 1)th derivative is monotonic
      if m == 0 then [] else solveMonotonic derivatives (m - 1) tRange
    else do
      -- We found a zero of the mth derivative in the interior of this subdomain
      let neighborhood = Solve1d.neighborhood n (pointOn fn t0)
      bifurcate derivatives tRange t0 n neighborhood m

bifurcate ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Range Unitless ->
  Float ->
  Int ->
  Solve1d.Neighborhood units ->
  Int ->
  List Root
bifurcate derivatives tRange t0 n neighborhood r
  | r == 0 = [Solve1d.root t0 neighborhood]
  | otherwise = do
      let k = r - 1
      let fk = Stream.nth k derivatives
      let valueK = pointOn fk t0
      if Qty.abs valueK <= Solve1d.derivativeTolerance neighborhood k
        then bifurcate derivatives tRange t0 n neighborhood k
        else do
          -- Derivative order k is non-zero at t0,
          -- which means it must be monotonic to the left and right of t0;
          -- solve for roots in the left and right subdomains
          let (tLow, tHigh) = Range.endpoints tRange
          let tLeft = Range.from tLow t0
          let tRight = Range.from t0 tHigh
          let leftRoots = if t0 > tLow then solveMonotonic derivatives k tLeft else []
          let rightRoots = if t0 < tHigh then solveMonotonic derivatives k tRight else []
          -- Also see if there's a root *at* t0
          let neighborhoodK = Solve1d.neighborhood k valueK
          let root = findRoot derivatives t0 k neighborhoodK (k - 1)
          leftRoots + root + rightRoots

findRoot ::
  Tolerance units =>
  Stream (Curve1d units) ->
  Float ->
  Int ->
  Solve1d.Neighborhood units ->
  Int ->
  Maybe Root
findRoot derivatives t0 n neighborhood k
  | n == 0 = Nothing
  | otherwise = do
      let fk = Stream.nth k derivatives
      let valueK = pointOn fk t0
      if Qty.abs valueK <= Solve1d.derivativeTolerance neighborhood k
        then
          if k == 0
            then Just (Solve1d.root t0 neighborhood)
            else findRoot derivatives t0 n neighborhood (k - 1)
        else
          if k == 0
            then Nothing
            else findRoot derivatives t0 k (Solve1d.neighborhood k valueK) (k - 1)

integral :: Curve1d units -> Estimate units
integral curve = Estimate.new (Integral curve (derivative curve) Range.unit)
