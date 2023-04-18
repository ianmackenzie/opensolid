module Curve1d
  ( Curve1d (Curve1d)
  , IsCurve1d (..)
  , evaluate
  , segmentBounds
  , derivative
  , zero
  , constant
  , parameter
  , squared
  , sqrt
  , sin
  , cos
  , roots
  )
where

import Angle qualified
import Curve1d.Region (Region (Region))
import Curve1d.Region qualified as Region
import Curve1d.Root (Root (Root))
import Curve1d.Root qualified as Root
import Float qualified
import Generic qualified
import List qualified
import OpenSolid
import Qty qualified
import Quadrature qualified
import Range (Range (..))
import Range qualified
import Units (Radians, Unitless)
import Units qualified
import Vector2d (Vector2d)
import Vector3d (Vector3d)
import {-# SOURCE #-} VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} VectorCurve2d qualified
import {-# SOURCE #-} VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} VectorCurve3d qualified

class IsCurve1d curve units | curve -> units where
  evaluateImpl :: curve -> Float -> Qty units
  segmentBoundsImpl :: curve -> Range Unitless -> Range units
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

instance (units1 ~ units1', units2 ~ units2') => Units.Coercion units1 units2 (Curve1d units1') (Curve1d units2')

instance units ~ units' => ApproximateEquality (Curve1d units) (Curve1d units') units where
  curve1 ~= curve2 = isZero (curve1 - curve2)

instance units ~ units' => ApproximateEquality (Curve1d units) (Qty units') units where
  curve ~= value = isZero (curve - value)

instance IsCurve1d (Curve1d units) units where
  evaluateImpl = evaluate
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

evaluate :: Curve1d units -> Float -> Qty units
evaluate curve t =
  case curve of
    Curve1d c -> evaluateImpl c t
    Zero -> Qty.zero
    Constant x -> x
    Parameter -> t
    Negated c -> negate (evaluate c t)
    Sum c1 c2 -> evaluate c1 t + evaluate c2 t
    Difference c1 c2 -> evaluate c1 t - evaluate c2 t
    Product c1 c2 -> evaluate c1 t * evaluate c2 t
    Quotient c1 c2 -> evaluate c1 t / evaluate c2 t
    Squared c -> let x = evaluate c t in x * x
    SquareRoot c -> Qty.sqrt (evaluate c t)
    Sin c -> Angle.sin (evaluate c t)
    Cos c -> Angle.cos (evaluate c t)

segmentBounds :: Curve1d units -> Range Unitless -> Range units
segmentBounds curve t =
  case curve of
    Curve1d c -> segmentBoundsImpl c t
    Zero -> Range.constant Qty.zero
    Constant value -> Range.constant value
    Parameter -> t
    Negated c -> negate (segmentBounds c t)
    Sum c1 c2 -> segmentBounds c1 t + segmentBounds c2 t
    Difference c1 c2 -> segmentBounds c1 t - segmentBounds c2 t
    Product c1 c2 -> segmentBounds c1 t * segmentBounds c2 t
    Quotient c1 c2 -> segmentBounds c1 t / segmentBounds c2 t
    Squared c -> let x = segmentBounds c t in Range.squared x
    SquareRoot c -> Range.sqrt (segmentBounds c t)
    Sin c -> Range.sin (segmentBounds c t)
    Cos c -> Range.cos (segmentBounds c t)

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
isZero curve = List.all [evaluate curve t ~= Qty.zero | t <- Quadrature.parameterValues]

maxRootOrder :: Int
maxRootOrder = 4

----- ROOT FINDING -----

data IsZero = IsZero deriving (Eq, Show)

instance IsError IsZero where
  errorMessage IsZero = "Curve1d is zero everywhere"

roots :: Tolerance units => Curve1d units -> Result IsZero (List Root)
roots Zero = Error IsZero
roots (Constant value) = if value ~= Qty.zero then Error IsZero else Ok []
roots curve | isZero curve = Error IsZero
roots curve = do
  let (root0, x0) = solveEndpoint curve 0.0
  let (root1, x1) = solveEndpoint curve 1.0
  resolvedRegions <- regions (Range.from x0 x1) curve
  let mergedRegions = List.collapse Region.merge resolvedRegions
  let solutions = List.combine (solve curve 0) mergedRegions
  Ok (root0 ++ List.foldr prependRoot root1 solutions)
 where
  ?originalCurve = curve

data Solution
  = Solution Root Float
  | NonZero (Range Unitless) Sign
  deriving (Eq, Show)

prependRoot :: Solution -> List Root -> List Root
prependRoot (Solution root _) acc = root : acc
prependRoot (NonZero _ _) acc = acc

solve :: (Tolerance units, ?originalCurve :: Curve1d units) => Curve1d units -> Int -> Region -> List Solution
solve curveDerivative derivativeOrder region
  | derivativeOrder == region.nonZeroDerivativeOrder = [NonZero region.domain region.nonZeroDerivativeSign]
  | otherwise =
      let nextDerivative = derivative curveDerivative
          higherOrderSolutions = solve nextDerivative (derivativeOrder + 1) region

          -- Solve for a root of this derivative within a non-zero region of the next higher
          -- derivative
          lift (NonZero subdomain nextDerivativeSign)
            -- Minimum point on the derivative curve is positive, so must be entirely positive
            | minY > Qty.zero = [NonZero subdomain Positive]
            -- Maximum point on the derivative curve is negative, so must be entirely negative
            | maxY < Qty.zero = [NonZero subdomain Negative]
            -- Otherwise, solve for a root of the derivative curve by bisection. If that is also a root of
            -- the original curve, then report it as such. Otherwise, then to the left and right of the root
            -- are non-zero regions of the derivative curve.
            | otherwise =
                let rootX = bisectMonotonic curveDerivative minX maxX minY maxY
                 in if evaluate ?originalCurve rootX ~= Qty.zero
                      then
                        let root = Root rootX derivativeOrder nextDerivativeSign
                            width = computeWidth (derivativeOrder + 1) (evaluate nextDerivative rootX)
                         in [Solution root width]
                      else
                        [ NonZero (Range.from x1 rootX) -nextDerivativeSign
                        , NonZero (Range.from rootX x2) nextDerivativeSign
                        ]
           where
            Range x1 x2 = subdomain
            minX = if nextDerivativeSign == Positive then x1 else x2
            maxX = if nextDerivativeSign == Positive then x2 else x1
            minY = evaluate curveDerivative minX
            maxY = evaluate curveDerivative maxX

          -- Check if a high-order root should in fact be a lower-order root (e.g. in y=x^3+x
          -- the 3rd derivative is zero at x=0 but it is in fact a 0th-order root, not a
          -- 2nd-order root)
          lift (Solution currentRoot currentWidth) =
            let rootX = currentRoot.value
                rootY = evaluate curveDerivative rootX
                width = computeWidth derivativeOrder rootY
             in if width < currentWidth
                  then [Solution (Root rootX (derivativeOrder - 1) (Qty.sign rootY)) width]
                  else [Solution currentRoot currentWidth]
       in List.combine lift higherOrderSolutions

bisectMonotonic :: Curve1d units -> Float -> Float -> Qty units -> Qty units -> Float
bisectMonotonic curve lowX highX lowY highY =
  let midX = Qty.midpoint lowX highX
   in if midX == lowX || midX == highX
        then if -lowY <= highY then lowX else highX
        else
          let midY = evaluate curve midX
           in if midY >= Qty.zero
                then bisectMonotonic curve lowX midX lowY midY
                else bisectMonotonic curve midX highX midY highY

regions :: Tolerance units => Range Unitless -> Curve1d units -> Result IsZero (List Region)
regions domain curve =
  case resolve domain curve of
    Ok region -> Ok [region]
    Error Indeterminate -> do
      (leftDomain, rightDomain) <- bisect domain
      leftRegions <- regions leftDomain curve
      rightRegions <- regions rightDomain curve
      Ok (leftRegions ++ rightRegions)

bisect :: Range Unitless -> Result IsZero (Range Unitless, Range Unitless)
bisect domain = if domain.isAtomic then Error IsZero else Ok (Range.bisect domain)

resolve :: Tolerance units => Range Unitless -> Curve1d units -> Result Indeterminate Region
resolve domain curve
  | curveBounds.minValue >= ?tolerance = Ok (Region domain 0 Positive)
  | curveBounds.maxValue <= negate ?tolerance = Ok (Region domain 0 Negative)
  | otherwise = resolveDerivative domain (derivative curve) 1
 where
  curveBounds = segmentBounds curve domain

resolved :: Range units -> Bool
resolved range = Range.resolution range >= 0.5

resolveDerivative :: Range Unitless -> Curve1d units -> Int -> Result Indeterminate Region
resolveDerivative domain curveDerivative derivativeOrder
  | resolved derivativeBounds =
      Ok (Region domain derivativeOrder (Qty.sign derivativeBounds.minValue))
  | derivativeOrder <= maxRootOrder =
      resolveDerivative domain (derivative curveDerivative) (derivativeOrder + 1)
  | otherwise = Error Indeterminate
 where
  derivativeBounds = segmentBounds curveDerivative domain

solveEndpoint :: Tolerance units => Curve1d units -> Float -> (List Root, Float)
solveEndpoint curve endpointX
  | evaluate curve endpointX ~= Qty.zero =
      let check curveDerivative derivativeOrder currentMinWidth currentBest =
            let derivativeValue = evaluate curveDerivative endpointX
                rootWidth = computeWidth derivativeOrder derivativeValue
                updatedMinWidth = min rootWidth currentMinWidth
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
  let domain = Range.from endpointX innerX
      derivativeBounds = segmentBounds curveDerivative domain
   in if resolved derivativeBounds
        then ([root], innerX)
        else
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
