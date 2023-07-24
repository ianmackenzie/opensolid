module Curve1d
  ( Curve1d (Curve1d)
  , IsCurve1d (..)
  , evaluateAt
  , pointOn
  , segmentBounds
  , derivative
  , zero
  , constant
  , parameter
  , squared
  , sqrt
  , sin
  , cos
  , ZeroEverywhere (ZeroEverywhere)
  , roots
  , EqualEverywhere (EqualEverywhere)
  , equalTo
  , equalToSquared
  , reverse
  , integral
  )
where

import Angle qualified
import Bisection qualified
import Curve1d.Integral (Integral (Integral))
import Curve1d.Root (Root (Root))
import Curve1d.Root qualified as Root
import Domain (Domain)
import Domain qualified
import Estimate (Estimate)
import Estimate qualified
import Float qualified
import Generic qualified
import List qualified
import OpenSolid
import Qty qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Stream (Stream)
import Stream qualified
import Units (Radians, Squared, Unitless)
import Units qualified
import Vector2d (Vector2d)
import Vector3d (Vector3d)
import {-# SOURCE #-} VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} VectorCurve2d qualified
import {-# SOURCE #-} VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} VectorCurve3d qualified

class Show curve => IsCurve1d curve units | curve -> units where
  evaluateAtImpl :: Float -> curve -> Qty units
  segmentBoundsImpl :: Domain -> curve -> Range units
  derivativeImpl :: curve -> Curve1d units

data Curve1d units where
  Curve1d :: forall curve units. IsCurve1d curve units => curve -> Curve1d units
  Zero :: Curve1d units
  Constant :: Qty units -> Curve1d units
  Parameter :: Curve1d Unitless
  Negated :: Curve1d units -> Curve1d units
  Sum :: Curve1d units -> Curve1d units -> Curve1d units
  Difference :: Curve1d units -> Curve1d units -> Curve1d units
  Product :: forall units1 units2 units3. Units.Product units1 units2 units3 => Curve1d units1 -> Curve1d units2 -> Curve1d units3
  Quotient :: forall units1 units2 units3. Units.Quotient units1 units2 units3 => Curve1d units1 -> Curve1d units2 -> Curve1d units3
  Squared :: forall units1 units2. Units.Squared units1 units2 => Curve1d units1 -> Curve1d units2
  SquareRoot :: forall units1 units2. Units.Squared units1 units2 => Curve1d units2 -> Curve1d units1
  Sin :: Curve1d Radians -> Curve1d Unitless
  Cos :: Curve1d Radians -> Curve1d Unitless

deriving instance Show (Curve1d units)

instance (units1 ~ units1', units2 ~ units2') => Units.Coercion units1 units2 (Curve1d units1') (Curve1d units2')

instance units ~ units' => ApproximateEquality (Curve1d units) (Curve1d units') units where
  curve1 ~= curve2 = isZero (curve1 - curve2)

instance units ~ units' => ApproximateEquality (Curve1d units) (Qty units') units where
  curve ~= value = isZero (curve - value)

instance IsCurve1d (Curve1d units) units where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative

zero :: Curve1d units
zero = Zero

constant :: Qty units -> Curve1d units
constant value = if value == Qty.zero then Zero else Constant value

parameter :: Curve1d Unitless
parameter = Parameter

instance Generic.Zero (Curve1d units) where
  zero = Zero

instance Negation (Curve1d units) where
  negate Zero = Zero
  negate (Constant x) = Constant (negate x)
  negate (Negated curve) = curve
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product c1 c2) = negate c1 * c2
  negate curve = Negated curve

instance Multiplication Sign (Curve1d units) (Curve1d units) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (Curve1d units) Sign (Curve1d units) where
  curve * Positive = curve
  curve * Negative = -curve

instance units ~ units' => Addition (Curve1d units) (Curve1d units') (Curve1d units) where
  Zero + curve = curve
  curve + Zero = curve
  Constant x + Constant y = constant (x + y)
  curve1 + curve2 = Sum curve1 curve2

instance units ~ units' => Addition (Curve1d units) (Qty units') (Curve1d units) where
  curve + value = curve + constant value

instance units ~ units' => Addition (Qty units) (Curve1d units') (Curve1d units) where
  value + curve = constant value + curve

instance units ~ units' => Subtraction (Curve1d units) (Curve1d units') (Curve1d units) where
  Zero - curve = negate curve
  curve - Zero = curve
  Constant x - Constant y = constant (x - y)
  curve1 - curve2 = Difference curve1 curve2

instance units ~ units' => Subtraction (Curve1d units) (Qty units') (Curve1d units) where
  curve - value = curve - constant value

instance units ~ units' => Subtraction (Qty units) (Curve1d units') (Curve1d units) where
  value - curve = constant value - curve

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3) where
  Zero * _ = Zero
  _ * Zero = Zero
  Constant x * Constant y = Constant (x * y)
  Constant x * curve | Units.drop x == 1.0 = Units.add (Units.drop curve)
  Constant x * curve | Units.drop x == -1.0 = Units.add (Units.drop (negate curve))
  Constant x * Negated c = negate x * c
  c1 * (Constant x) = Constant x * c1
  Constant x * Product (Constant y) c = Units.add (Product (Constant (Units.drop x * Units.drop y)) (Units.drop c))
  curve1 * curve2 = Product curve1 curve2

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (Qty units2) (Curve1d units3) where
  curve * value = curve * constant value

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (Curve1d units2) (Curve1d units3) where
  value * curve = constant value * curve

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (Vector2d (space @ units2)) (VectorCurve2d (space @ units3)) where
  curve * vector = curve * VectorCurve2d.constant vector

instance Units.Product units1 units2 units3 => Multiplication (Vector2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3)) where
  vector * curve = VectorCurve2d.constant vector * curve

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (Vector3d (space @ units2)) (VectorCurve3d (space @ units3)) where
  curve * vector = curve * VectorCurve3d.constant vector

instance Units.Product units1 units2 units3 => Multiplication (Vector3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3)) where
  vector * curve = VectorCurve3d.constant vector * curve

instance Units.Quotient units1 units2 units3 => Division (Curve1d units1) (Curve1d units2) (Curve1d units3) where
  Zero / _ = Zero
  Constant x / Constant y = Constant (x / y)
  curve / Constant x = Units.specialize ((Units.generalize 1.0 ./ Units.generalize x) .* Units.generalize curve)
  curve1 / curve2 = Quotient curve1 curve2

instance Units.Quotient units1 units2 units3 => Division (Curve1d units1) (Qty units2) (Curve1d units3) where
  curve / value = curve / constant value

instance Units.Quotient units1 units2 units3 => Division (Qty units1) (Curve1d units2) (Curve1d units3) where
  value / curve = constant value / curve

evaluateAt :: Float -> Curve1d units -> Qty units
evaluateAt t curve =
  case curve of
    Curve1d c -> evaluateAtImpl t c
    Zero -> Qty.zero
    Constant x -> x
    Parameter -> t
    Negated c -> negate (evaluateAt t c)
    Sum c1 c2 -> evaluateAt t c1 + evaluateAt t c2
    Difference c1 c2 -> evaluateAt t c1 - evaluateAt t c2
    Product c1 c2 -> evaluateAt t c1 * evaluateAt t c2
    Quotient c1 c2 -> evaluateAt t c1 / evaluateAt t c2
    Squared c -> let x = evaluateAt t c in x * x
    SquareRoot c -> Qty.sqrt (evaluateAt t c)
    Sin c -> Angle.sin (evaluateAt t c)
    Cos c -> Angle.cos (evaluateAt t c)

pointOn :: Curve1d units -> Float -> Qty units
pointOn curve t = evaluateAt t curve

segmentBounds :: Domain -> Curve1d units -> Range units
segmentBounds t curve =
  case curve of
    Curve1d c -> segmentBoundsImpl t c
    Zero -> Range.constant Qty.zero
    Constant value -> Range.constant value
    Parameter -> t
    Negated c -> negate (segmentBounds t c)
    Sum c1 c2 -> segmentBounds t c1 + segmentBounds t c2
    Difference c1 c2 -> segmentBounds t c1 - segmentBounds t c2
    Product c1 c2 -> segmentBounds t c1 * segmentBounds t c2
    Quotient c1 c2 -> segmentBounds t c1 / segmentBounds t c2
    Squared c -> let x = segmentBounds t c in Range.squared x
    SquareRoot c -> Range.sqrt (segmentBounds t c)
    Sin c -> Range.sin (segmentBounds t c)
    Cos c -> Range.cos (segmentBounds t c)

derivative :: Curve1d units -> Curve1d units
derivative curve =
  case curve of
    Curve1d c -> derivativeImpl c
    Zero -> zero
    Constant _ -> zero
    Parameter -> constant 1.0
    Negated c -> negate (derivative c)
    Sum c1 c2 -> derivative c1 + derivative c2
    Difference c1 c2 -> derivative c1 - derivative c2
    Product c1 c2 -> derivative c1 * c2 + c1 * derivative c2
    Quotient c1 c2 -> derivative c1 / c2 + curve * (derivative c2 / c2)
    Squared c -> 2.0 * c * derivative c
    SquareRoot c -> derivative c / (2.0 * sqrt c)
    Sin c -> cos c * Units.drop (derivative c)
    Cos c -> negate (sin c) * Units.drop (derivative c)

newtype Reversed units = Reversed (Curve1d units)

deriving instance Show (Reversed units)

instance IsCurve1d (Reversed units) units where
  evaluateAtImpl t (Reversed curve) = evaluateAt (1.0 - t) curve
  segmentBoundsImpl t (Reversed curve) = segmentBounds (1.0 - t) curve
  derivativeImpl (Reversed curve) = -(reverse (derivative curve))

reverse :: Curve1d units -> Curve1d units
reverse Zero = Zero
reverse curve@(Constant _) = curve
reverse curve = Curve1d (Reversed curve)

squared :: Units.Squared units1 units2 => Curve1d units1 -> Curve1d units2
squared Zero = Zero
squared (Constant x) = Constant (x * x)
squared (Negated c) = squared c
squared (Cos c) = Units.add (cosSquared c)
squared (Sin c) = Units.add (sinSquared c)
squared curve = Squared curve

cosSquared :: Curve1d Radians -> Curve1d Unitless
cosSquared c = 0.5 * cos (2.0 * c) + 0.5

sinSquared :: Curve1d Radians -> Curve1d Unitless
sinSquared c = 0.5 - 0.5 * cos (2.0 * c)

sqrt :: Units.Squared units1 units2 => Curve1d units2 -> Curve1d units1
sqrt Zero = Zero
sqrt (Constant x) = Constant (Qty.sqrt x)
sqrt curve = SquareRoot curve

sin :: Curve1d Radians -> Curve1d Unitless
sin Zero = Zero
sin (Constant x) = constant (Angle.sin x)
sin curve = Sin curve

cos :: Curve1d Radians -> Curve1d Unitless
cos Zero = Constant 1.0
cos (Constant x) = constant (Angle.cos x)
cos curve = Cos curve

isZero :: Tolerance units => Curve1d units -> Bool
isZero curve = List.all (~= Qty.zero) (Domain.sample (pointOn curve) Domain.unit)

maxRootOrder :: Int
maxRootOrder = 4

----- ROOT FINDING -----

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, ErrorMessage)

data EqualEverywhere = EqualEverywhere deriving (Eq, Show, ErrorMessage)

equalTo :: Tolerance units => Qty units -> Curve1d units -> Result EqualEverywhere (List Root)
equalTo value curve =
  roots (curve - value) |> Result.mapError (\ZeroEverywhere -> EqualEverywhere)

equalToSquared ::
  (Tolerance units1, Squared units1 units2) =>
  Qty units1 ->
  Curve1d units2 ->
  Result EqualEverywhere (List Root)
equalToSquared value curve =
  let ?tolerance = ?tolerance * ?tolerance + 2.0 * value * ?tolerance
   in roots (curve - Qty.squared value) |> Result.mapError (\ZeroEverywhere -> EqualEverywhere)

roots :: Tolerance units => Curve1d units -> Result ZeroEverywhere (List Root)
roots Zero = Error ZeroEverywhere
roots (Constant value) = if value ~= Qty.zero then Error ZeroEverywhere else Ok []
roots curve | isZero curve = Error ZeroEverywhere
roots curve =
  let (root0, x0) = solveEndpoint curve 0.0
      (root1, x1) = solveEndpoint curve 1.0
      derivatives = Stream.iterate curve derivative
      searchTree = Bisection.tree (\domain -> Stream.map (segmentBounds domain) derivatives)
      endpointRoots = root0 ++ root1
      endpointExclusions = [Range.from 0.0 x0, Range.from x1 1.0]
      (allRoots, _) =
        (endpointRoots, endpointExclusions)
          |> findRoots curve derivatives searchTree maxRootOrder
   in Ok (List.sortBy Root.value allRoots)

findRoots ::
  Tolerance units =>
  Curve1d units ->
  Stream (Curve1d units) ->
  Bisection.Tree (Stream (Range units)) ->
  Int ->
  (List Root, List Domain) ->
  (List Root, List Domain)
findRoots curve derivatives searchTree rootOrder accumulated =
  let updated =
        Bisection.solve
          (isCandidate rootOrder)
          (resolveDerivativeSign (rootOrder + 1))
          (findRoot curve rootOrder derivatives)
          searchTree
          accumulated
   in if rootOrder == 0
        then updated
        else findRoots curve derivatives searchTree (rootOrder - 1) updated

isCandidate :: Tolerance units => Int -> Domain -> Stream (Range units) -> Bool
isCandidate rootOrder _ bounds =
  let curveBounds = Stream.head bounds
      derivativeBounds = Stream.take rootOrder (Stream.tail bounds)
      curveContainsZero = Range.approximatelyIncludes Qty.zero curveBounds
      derivativesContainZero = List.all (Range.includes Qty.zero) derivativeBounds
   in curveContainsZero && derivativesContainZero

resolveDerivativeSign :: Int -> Domain -> Stream (Range units) -> Fuzzy Sign
resolveDerivativeSign derivativeOrder _ bounds = resolveSign (Stream.nth derivativeOrder bounds)

findRoot ::
  Tolerance units =>
  Curve1d units ->
  Int ->
  Stream (Curve1d units) ->
  Domain ->
  Stream (Range units) ->
  Sign ->
  Maybe Root
findRoot originalCurve rootOrder derivatives domain _ nextDerivativeSign =
  let curve = Stream.nth rootOrder derivatives
      Range x1 x2 = domain
      minX = if nextDerivativeSign == Positive then x1 else x2
      maxX = if nextDerivativeSign == Positive then x2 else x1
      minY = evaluateAt minX curve
      maxY = evaluateAt maxX curve
   in if minY > Qty.zero || maxY < Qty.zero
        then Nothing
        else
          let rootX = bisectMonotonic curve minX maxX minY maxY
           in if rootOrder == 0 || evaluateAt rootX originalCurve ~= Qty.zero
                then Just (Root rootX rootOrder nextDerivativeSign)
                else Nothing

bisectMonotonic :: Curve1d units -> Float -> Float -> Qty units -> Qty units -> Float
bisectMonotonic curve lowX highX lowY highY =
  let midX = Qty.midpoint lowX highX
   in if midX == lowX || midX == highX
        then if -lowY <= highY then lowX else highX
        else
          let midY = evaluateAt midX curve
           in if midY >= Qty.zero
                then bisectMonotonic curve lowX midX lowY midY
                else bisectMonotonic curve midX highX midY highY

resolveSign :: Range units -> Fuzzy Sign
resolveSign range
  | resolution >= 0.5 = Resolved Positive
  | resolution <= -0.5 = Resolved Negative
  | otherwise = Unresolved
 where
  resolution = Range.resolution range

solveEndpoint :: Tolerance units => Curve1d units -> Float -> (List Root, Float)
solveEndpoint curve endpointX
  | evaluateAt endpointX curve ~= Qty.zero =
      let check curveDerivative derivativeOrder currentMinWidth currentBest =
            let derivativeValue = evaluateAt endpointX curveDerivative
                rootWidth = computeWidth derivativeOrder derivativeValue
                updatedMinWidth = Float.min rootWidth currentMinWidth
                rootOrder = derivativeOrder - 1
                updatedBest =
                  if updatedMinWidth < currentMinWidth
                    then Just (Root endpointX rootOrder (Qty.sign derivativeValue), curveDerivative)
                    else currentBest
             in if rootOrder < maxRootOrder
                  then check (derivative curveDerivative) (derivativeOrder + 1) updatedMinWidth updatedBest
                  else case updatedBest of
                    Just (root, associatedDerivative) -> resolveEndpoint root associatedDerivative endpointX 0.5
                    Nothing -> ([], endpointX)
       in check (derivative curve) 1 Qty.infinity Nothing
  | otherwise = ([], endpointX)

resolveEndpoint :: Root -> Curve1d units -> Float -> Float -> (List Root, Float)
resolveEndpoint root curveDerivative endpointX innerX =
  case resolveSign (segmentBounds (Range.from endpointX innerX) curveDerivative) of
    Resolved _ -> ([root], innerX)
    Unresolved ->
      let midX = Qty.midpoint endpointX innerX
       in if midX == endpointX || midX == innerX
            then ([], endpointX)
            else resolveEndpoint root curveDerivative endpointX midX

computeWidth :: Tolerance units => Int -> Qty units -> Float
computeWidth 1 derivativeValue = ?tolerance / Qty.abs derivativeValue
computeWidth 2 derivativeValue = Qty.sqrt (2 * ?tolerance / Qty.abs derivativeValue)
computeWidth derivativeOrder derivativeValue =
  Float.pow
    (factorial derivativeOrder * ?tolerance / Qty.abs derivativeValue)
    (1.0 / Float.fromInt derivativeOrder)

factorial :: Int -> Int
factorial 0 = 1; factorial n = n * factorial (n - 1)

integral :: Curve1d units -> Estimate units
integral curve = Estimate.wrap (Integral curve (derivative curve) Domain.unit)
