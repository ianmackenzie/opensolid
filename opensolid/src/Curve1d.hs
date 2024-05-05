module Curve1d
  ( Curve1d
  , Interface (..)
  , evaluateAt
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
  , hasInternalZero
  , Zeros (..)
  , zeros
  , reverse
  , integral
  )
where

import Angle qualified
import Bisection qualified
import Curve1d.Integral (Integral (Integral))
import Curve1d.Root (Root (Root))
import Curve1d.Root qualified as Root
import Estimate (Estimate)
import Estimate qualified
import Float qualified
import Int qualified
import List qualified
import Maybe qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Stream (Stream)
import Stream qualified
import Units qualified

class Show curve => Interface curve units | curve -> units where
  evaluateAtImpl :: Float -> curve -> Qty units
  segmentBoundsImpl :: Range Unitless -> curve -> Range units
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
  evaluateAtImpl = evaluateAt
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

instance Product Sign (Curve1d units) (Curve1d units)

instance Multiplication Sign (Curve1d units) where
  type Sign .*. Curve1d units = Curve1d (Unitless :*: units)
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Product (Curve1d units) Sign (Curve1d units)

instance Multiplication (Curve1d units) Sign where
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
  Product (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Multiplication (Curve1d units1) (Curve1d units2) where
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

instance
  Units.Product units1 units2 units3 =>
  Product (Curve1d units1) (Qty units2) (Curve1d units3)

instance Multiplication (Curve1d units1) (Qty units2) where
  type Curve1d units1 .*. Qty units2 = Curve1d (units1 :*: units2)
  curve .*. value = curve .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Product (Qty units1) (Curve1d units2) (Curve1d units3)

instance Multiplication (Qty units1) (Curve1d units2) where
  type Qty units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  value .*. curve = constant value .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Division (Curve1d units1) (Curve1d units2) where
  type Curve1d units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  Constant (Qty 0.0) ./. _ = zero
  Constant x ./. Constant y = Constant (x ./. y)
  curve ./. Constant x = (1.0 ./. x) .*^ curve
  curve1 ./. curve2 = Quotient' curve1 curve2

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (Curve1d units1) (Qty units2) (Curve1d units3)

instance Division (Curve1d units1) (Qty units2) where
  type Curve1d units1 ./. Qty units2 = Curve1d (units1 :/: units2)
  curve ./. value = curve ./. constant value

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (Qty units1) (Curve1d units2) (Curve1d units3)

instance Division (Qty units1) (Curve1d units2) where
  type Qty units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  value ./. curve = constant value ./. curve

evaluateAt :: Float -> Curve1d units -> Qty units
evaluateAt tValue curve =
  case curve of
    Curve1d c -> evaluateAtImpl tValue c
    Constant x -> x
    Parameter -> tValue
    Negated c -> negate (evaluateAt tValue c)
    Sum c1 c2 -> evaluateAt tValue c1 + evaluateAt tValue c2
    Difference c1 c2 -> evaluateAt tValue c1 - evaluateAt tValue c2
    Product' c1 c2 -> evaluateAt tValue c1 .*. evaluateAt tValue c2
    Quotient' c1 c2 -> evaluateAt tValue c1 ./. evaluateAt tValue c2
    Squared' c -> Qty.squared' (evaluateAt tValue c)
    SquareRoot' c' -> Qty.sqrt' (evaluateAt tValue c')
    Sin c -> Angle.sin (evaluateAt tValue c)
    Cos c -> Angle.cos (evaluateAt tValue c)
    Coerce c -> Units.coerce (evaluateAt tValue c)

pointOn :: Curve1d units -> Float -> Qty units
pointOn curve tValue = evaluateAt tValue curve

segmentBounds :: Range Unitless -> Curve1d units -> Range units
segmentBounds tBounds curve =
  case curve of
    Curve1d c -> segmentBoundsImpl tBounds c
    Constant value -> Range.constant value
    Parameter -> tBounds
    Negated c -> negate (segmentBounds tBounds c)
    Sum c1 c2 -> segmentBounds tBounds c1 + segmentBounds tBounds c2
    Difference c1 c2 -> segmentBounds tBounds c1 - segmentBounds tBounds c2
    Product' c1 c2 -> segmentBounds tBounds c1 .*. segmentBounds tBounds c2
    Quotient' c1 c2 -> segmentBounds tBounds c1 ./. segmentBounds tBounds c2
    Squared' c -> Range.squared' (segmentBounds tBounds c)
    SquareRoot' c' -> Range.sqrt' (segmentBounds tBounds c')
    Sin c -> Range.sin (segmentBounds tBounds c)
    Cos c -> Range.cos (segmentBounds tBounds c)
    Coerce c -> Units.coerce (segmentBounds tBounds c)

derivative :: Curve1d units -> Curve1d units
derivative curve =
  case curve of
    Curve1d c -> derivativeImpl c
    Constant _ -> zero
    Parameter -> constant 1.0
    Negated c -> negate (derivative c)
    Sum c1 c2 -> derivative c1 + derivative c2
    Difference c1 c2 -> derivative c1 - derivative c2
    Product' c1 c2 -> derivative c1 .*. c2 + c1 .*. derivative c2
    Quotient' c1 c2 -> (derivative c1 .*. c2 - c1 .*. derivative c2) .!/.! squared' c2
    Squared' c -> 2.0 * c .*. derivative c
    SquareRoot' c' -> derivative c' .!/! (2.0 * sqrt' c')
    Sin c -> cos c * Angle.unitless (derivative c)
    Cos c -> negate (sin c) * Angle.unitless (derivative c)
    Coerce c -> Units.coerce (derivative c)

newtype Reversed units = Reversed (Curve1d units)

deriving instance Show (Reversed units)

instance Interface (Reversed units) units where
  evaluateAtImpl tValue (Reversed curve) = evaluateAt (1.0 - tValue) curve
  segmentBoundsImpl tBounds (Reversed curve) = segmentBounds (1.0 - tBounds) curve
  derivativeImpl (Reversed curve) = -(reverse (derivative curve))

reverse :: Curve1d units -> Curve1d units
reverse curve@(Constant _) = curve
reverse curve = Curve1d (Reversed curve)

squared :: Units.Squared units1 units2 => Curve1d units1 -> Curve1d units2
squared curve = Units.specialize (squared' curve)

squared' :: Curve1d units -> Curve1d (units :*: units)
squared' (Constant x) = Constant (x .*. x)
squared' (Negated c) = squared' c
squared' (Cos c) = Units.unspecialize (cosSquared c)
squared' (Sin c) = Units.unspecialize (sinSquared c)
squared' curve = Squared' curve

cosSquared :: Curve1d Radians -> Curve1d Unitless
cosSquared c = 0.5 * cos (2.0 * c) + 0.5

sinSquared :: Curve1d Radians -> Curve1d Unitless
sinSquared c = 0.5 - 0.5 * cos (2.0 * c)

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
isZero curve = List.all (pointOn curve >> (~= Qty.zero)) (Range.samples Range.unit)

-- TODO report an error if higher-order root detected
maxRootOrder :: Int
maxRootOrder = 4

----- ROOT FINDING -----

hasInternalZero :: Tolerance units => Curve1d units -> Bool
hasInternalZero curve = isZero curve || findInternalZero curve Range.unit

findInternalZero :: Tolerance units => Curve1d units -> Range Unitless -> Bool
findInternalZero curve domain = do
  let curveBounds = segmentBounds domain curve
  if
    | not (curveBounds ^ Qty.zero) -> False
    | curveBounds ~= Qty.zero -> not (Range.includes 0.0 domain || Range.includes 1.0 domain)
    | otherwise -> do
        let (left, right) = Range.bisect domain
        findInternalZero curve left || findInternalZero curve right

data Zeros = ZeroEverywhere | Zeros (List Root)

zeros :: Tolerance units => Curve1d units -> Zeros
zeros (Constant value) = if value ~= Qty.zero then ZeroEverywhere else Zeros []
zeros curve | isZero curve = ZeroEverywhere
zeros curve = do
  let (root0, x0) = solveEndpoint curve 0.0
  let (root1, x1) = solveEndpoint curve 1.0
  let derivatives = Stream.iterate curve derivative
  let searchTree = Bisection.tree (\domain -> Stream.map (segmentBounds domain) derivatives)
  let endpointRoots = root0 + root1
  let endpointExclusions = [Range.from 0.0 x0, Range.from x1 1.0]
  let (allRoots, _) = findRoots curve derivatives searchTree maxRootOrder (endpointRoots, endpointExclusions)
  Zeros (List.sortBy Root.value allRoots)

findRoots ::
  Tolerance units =>
  Curve1d units ->
  Stream (Curve1d units) ->
  Bisection.Tree (Stream (Range units)) ->
  Int ->
  (List Root, List (Range Unitless)) ->
  (List Root, List (Range Unitless))
findRoots curve derivatives searchTree rootOrder accumulated = do
  let updated =
        Bisection.solve
          (isCandidate rootOrder)
          (resolveDerivativeSign (rootOrder + 1))
          (findRoot curve rootOrder derivatives)
          searchTree
          accumulated
  if rootOrder == 0
    then updated
    else findRoots curve derivatives searchTree (rootOrder - 1) updated

isCandidate :: Tolerance units => Int -> Range Unitless -> Stream (Range units) -> Bool
isCandidate rootOrder _ bounds = do
  let curveBounds = Stream.head bounds
  let derivativeBounds = Stream.take rootOrder (Stream.tail bounds)
  let curveContainsZero = curveBounds ^ Qty.zero
  let derivativesContainZero = List.all (Range.includes Qty.zero) derivativeBounds
  curveContainsZero && derivativesContainZero

resolveDerivativeSign :: Int -> Range Unitless -> Stream (Range units) -> Fuzzy Sign
resolveDerivativeSign derivativeOrder _ bounds = resolveSign (Stream.nth derivativeOrder bounds)

findRoot ::
  Tolerance units =>
  Curve1d units ->
  Int ->
  Stream (Curve1d units) ->
  Range Unitless ->
  Stream (Range units) ->
  Sign ->
  Maybe Root
findRoot originalCurve rootOrder derivatives domain _ nextDerivativeSign = Maybe.do
  let curve = Stream.nth rootOrder derivatives
  rootX <- Range.solve (pointOn curve) domain
  if rootOrder == 0 || evaluateAt rootX originalCurve ~= Qty.zero
    then Just (Root{value = rootX, order = rootOrder, sign = nextDerivativeSign})
    else Nothing

resolveSign :: Range units -> Fuzzy Sign
resolveSign range = do
  let resolution = Range.resolution range
  if
    | resolution >= 0.5 -> Resolved Positive
    | resolution <= -0.5 -> Resolved Negative
    | otherwise -> Unresolved

solveEndpoint :: Tolerance units => Curve1d units -> Float -> (Maybe Root, Float)
solveEndpoint curve endpointX
  | evaluateAt endpointX curve ~= Qty.zero = do
      let check curveDerivative derivativeOrder currentMinWidth currentBest = do
            let derivativeValue = evaluateAt endpointX curveDerivative
            let rootWidth = computeWidth derivativeOrder derivativeValue
            let updatedMinWidth = Float.min rootWidth currentMinWidth
            let rootOrder = derivativeOrder - 1
            let updatedBest =
                  if updatedMinWidth < currentMinWidth
                    then
                      Just
                        ( Root{value = endpointX, order = rootOrder, sign = Qty.sign derivativeValue}
                        , curveDerivative
                        )
                    else currentBest
            if rootOrder < maxRootOrder
              then
                check
                  (derivative curveDerivative)
                  (derivativeOrder + 1)
                  updatedMinWidth
                  updatedBest
              else case updatedBest of
                Just (root, associatedDerivative) ->
                  resolveEndpoint root associatedDerivative endpointX 0.5
                Nothing -> (Nothing, endpointX)
      check (derivative curve) 1 Qty.infinity Nothing
  | otherwise = (Nothing, endpointX)

resolveEndpoint :: Root -> Curve1d units -> Float -> Float -> (Maybe Root, Float)
resolveEndpoint root curveDerivative endpointX innerX =
  case resolveSign (segmentBounds (Range.from endpointX innerX) curveDerivative) of
    Resolved _ -> (Just root, innerX)
    Unresolved -> do
      let midX = Qty.midpoint endpointX innerX
      if midX == endpointX || midX == innerX
        then (Nothing, endpointX)
        else resolveEndpoint root curveDerivative endpointX midX

computeWidth :: Tolerance units => Int -> Qty units -> Float
computeWidth derivativeOrder derivativeValue = do
  let ratio = Qty.abs (?tolerance / derivativeValue)
  case derivativeOrder of
    1 -> ratio
    2 -> Qty.sqrt (2 * ratio)
    n -> (Int.factorial n * ratio) ** (1 / n)

integral :: Curve1d units -> Estimate units
integral curve = Estimate.wrap (Integral curve (derivative curve) Range.unit)
