module Curve1d
  ( Curve1d
  , Interface (..)
  , pointOn
  , segmentBounds
  , derivative
  , wrap
  , zero
  , constant
  , t
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  , Zeros (ZeroEverywhere, Zeros)
  , HigherOrderZero (HigherOrderZero)
  , zeros
  , reverse
  , integral
  )
where

import Angle qualified
import Curve1d.Integral (Integral (Integral))
import Curve1d.Root (Root (Root))
import Curve1d.Root qualified as Root
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
import Result qualified
import Solve1d qualified
import Stream (Stream (Stream))
import Stream qualified
import Tolerance qualified
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

instance Units.Coercion (Curve1d units1) (Curve1d units2) where
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

wrap :: Interface curve units => curve -> Curve1d units
wrap = Curve1d

zero :: Curve1d units
zero = constant Qty.zero

constant :: Qty units -> Curve1d units
constant = Constant

t :: Curve1d Unitless
t = Parameter

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
isZero curve = case curve of
  Constant value -> value ~= Qty.zero
  _ -> List.allTrue [pointOn curve tValue ~= Qty.zero | tValue <- Parameter.samples]

----- ROOT FINDING -----

data HigherOrderZero = HigherOrderZero deriving (Eq, Show, Error)

data Zeros = ZeroEverywhere | Zeros (List Root) deriving (Show)

zeros :: Tolerance units => Curve1d units -> Result HigherOrderZero Zeros
zeros (Constant value) = if value ~= Qty.zero then Ok ZeroEverywhere else Ok (Zeros [])
zeros curve | isZero curve = Ok ZeroEverywhere
zeros curve = Result.do
  let derivatives = Stream.iterate curve derivative
  let cache =
        Solve1d.init $
          \range -> Stream.map (\curveDerivative -> segmentBounds curveDerivative range) derivatives
  (roots, _) <- findZeros derivatives [0 .. 3] cache
  Ok (Zeros (List.sortBy Root.value roots))

findZeros ::
  Tolerance units =>
  Stream (Curve1d units) ->
  List Int ->
  Solve1d.Cache (Stream (Range units)) ->
  Result HigherOrderZero (List Root, List Domain1d)
findZeros derivatives orders cache = case orders of
  [] -> Ok ([], []) -- No more orders to try
  n : higherOrders -> Result.do
    (higherOrderRoots, higherOrderExclusions) <- findZeros derivatives higherOrders cache
    Solve1d.search (resolveOrder n derivatives) cache higherOrderRoots higherOrderExclusions
      ?? Error HigherOrderZero

resolveOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve1d units) ->
  Domain1d ->
  Stream (Range units) ->
  Solve1d.Exclusions exclusions ->
  Solve1d.Action exclusions Root
resolveOrder n derivatives subdomain derivativeBounds exclusions
  -- The curve itself is non-zero, so no solution exists within this domain
  | not (Stream.head derivativeBounds ^ Qty.zero) = Solve1d.pass
  -- A lower-order derivative is non-zero, so no solution of the given order exists
  | anyResolved n (Stream.tail derivativeBounds) = Solve1d.pass
  | otherwise = case exclusions of
      -- We're overlapping at least one exclusion, so we need to bisect further
      Solve1d.SomeExclusions _ -> Solve1d.recurse
      -- No exclusions, so try to solve for a root of order n
      Solve1d.NoExclusions -> do
        -- For a solution of order n, we need to look at the derivative of order n + 1
        let nextDerivativeBounds = Stream.nth (n + 1) derivativeBounds
        case Range.resolvedSign nextDerivativeBounds of
          -- Next derivative is not resolved, need to bisect further
          Nothing -> Solve1d.recurse
          -- Next derivative *is* resolved, try to find a root of order n
          Just sign -> do
            let subdomainInterior = Domain1d.interior subdomain
            let neighborhood = Solve1d.neighborhood (n + 1) (Range.maxAbs nextDerivativeBounds)
            let isSolution = isSolutionOrder n neighborhood derivatives
            if
              | Range.includes 0.0 subdomainInterior && isSolution 0.0 ->
                  Solve1d.return (Root 0.0 n sign)
              | Range.includes 1.0 subdomainInterior && isSolution 1.0 ->
                  Solve1d.return (Root 1.0 n sign)
              | otherwise -> do
                  let fn = pointOn (Stream.nth n derivatives)
                  let fm = pointOn (Stream.nth (n + 1) derivatives)
                  let fnTolerance = Solve1d.derivativeTolerance neighborhood n
                  let subdomainBounds = Domain1d.bounds subdomain
                  let x = Tolerance.using fnTolerance (Solve1d.monotonic fn fm subdomainBounds)
                  if isSolutionOrder n neighborhood derivatives x
                    then
                      if Range.includes x subdomainInterior
                        then -- We've found a valid root of order n
                          Solve1d.return (Root x n sign)
                        else -- We found a root, but it's not in the interior of this subdomain
                          Solve1d.recurse
                    else Solve1d.pass -- No root of order n in this subdomain

anyResolved :: Int -> Stream (Range units) -> Bool
anyResolved n (Stream first rest) = n > 0 && (Range.isResolved first || anyResolved (n - 1) rest)

-- Check that the values of all derivatives of order n and lower
-- (including order 0, the curve itself)
-- are zero to within an appropriate tolerance
isSolutionOrder ::
  Tolerance units =>
  Int ->
  Solve1d.Neighborhood units ->
  Stream (Curve1d units) ->
  Float ->
  Bool
isSolutionOrder n neighborhood derivatives x = do
  let curve = Stream.head derivatives
  let curveIsZero = pointOn curve x ~= Qty.zero
  let curveDerivative k = Stream.nth k derivatives
  let derivativeValue k = pointOn (curveDerivative k) x
  let derivativeTolerance k = Solve1d.derivativeTolerance neighborhood k
  let derivativeIsZero k = Qty.abs (derivativeValue k) < derivativeTolerance k
  curveIsZero && List.allSatisfy derivativeIsZero [1 .. n]

integral :: Curve1d units -> Estimate units
integral curve = Estimate.wrap (Integral curve (derivative curve) Range.unit)
