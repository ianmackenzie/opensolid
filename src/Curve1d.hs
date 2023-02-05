module Curve1d
  ( Curve1d (Curve1d)
  , IsCurve1d
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
import Units qualified
import Vector2d (Vector2d)
import Vector3d (Vector3d)
import {-# SOURCE #-} VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} VectorCurve2d qualified
import {-# SOURCE #-} VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} VectorCurve3d qualified

class IsCurve1d curve units | curve -> units where
  pointOn :: curve -> Float -> Qty units
  segmentBounds :: curve -> Range Unitless -> Range units
  derivative :: curve -> Curve1d units

data Curve1d units where
  Curve1d :: forall curve units. IsCurve1d curve units => curve -> Curve1d units
  Zero :: Curve1d units
  Constant :: Qty units -> Curve1d units
  Parameter :: Curve1d Unitless
  Negated :: Curve1d units -> Curve1d units
  Sum :: Curve1d units -> Curve1d units -> Curve1d units
  Difference :: Curve1d units -> Curve1d units -> Curve1d units
  Product :: forall units1 units2 units3. Multiplication (Qty units1) (Qty units2) (Qty units3) => Curve1d units1 -> Curve1d units2 -> Curve1d units3
  Quotient :: forall units1 units2 units3. Division (Qty units1) (Qty units2) (Qty units3) => Curve1d units1 -> Curve1d units2 -> Curve1d units3
  Squared :: forall units1 units2. Squared (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
  SquareRoot :: forall units1 units2. Squared (Qty units1) (Qty units2) => Curve1d units2 -> Curve1d units1
  Sin :: Curve1d Radians -> Curve1d Unitless
  Cos :: Curve1d Radians -> Curve1d Unitless

instance Units.Coercion (Curve1d units) (Curve1d Unitless)

instance IsCurve1d (Curve1d units) units where
  pointOn curve t =
    case curve of
      Curve1d c -> pointOn c t
      Zero -> Qty.zero
      Constant x -> x
      Parameter -> t
      Negated c -> negate (pointOn c t)
      Sum c1 c2 -> pointOn c1 t + pointOn c2 t
      Difference c1 c2 -> pointOn c1 t - pointOn c2 t
      Product c1 c2 -> pointOn c1 t * pointOn c2 t
      Quotient c1 c2 -> pointOn c1 t / pointOn c2 t
      Squared c -> let x = pointOn c t in x * x
      SquareRoot c -> Qty.sqrt (pointOn c t)
      Sin c -> Angle.sin (pointOn c t)
      Cos c -> Angle.cos (pointOn c t)

  segmentBounds curve t =
    case curve of
      Curve1d c -> segmentBounds c t
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

  derivative curve =
    case curve of
      Curve1d c -> derivative c
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

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3) where
  Zero * _ = Zero
  _ * Zero = Zero
  Constant x * Constant y = Constant (x * y)
  Constant x * curve | Units.drop x == 1.0 = Units.add (Units.drop curve)
  Constant x * curve | Units.drop x == -1.0 = Units.add (Units.drop (negate curve))
  Constant x * Negated c = negate x * c
  c1 * (Constant x) = Constant x * c1
  Constant x * Product (Constant y) c = Units.add (Product (Constant (Units.drop x * Units.drop y)) (Units.drop c))
  curve1 * curve2 = Product curve1 curve2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Qty units2) (Curve1d units3) where
  curve * value = curve * constant value

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Curve1d units2) (Curve1d units3) where
  value * curve = constant value * curve

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Vector2d units2 coordinates) (VectorCurve2d units3 coordinates) where
  curve * vector = curve * VectorCurve2d.constant vector

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector2d units1 coordinates) (Curve1d units2) (VectorCurve2d units3 coordinates) where
  vector * curve = VectorCurve2d.constant vector * curve

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Vector3d units2 coordinates) (VectorCurve3d units3 coordinates) where
  curve * vector = curve * VectorCurve3d.constant vector

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector3d units1 coordinates) (Curve1d units2) (VectorCurve3d units3 coordinates) where
  vector * curve = VectorCurve3d.constant vector * curve

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Curve1d units1) (Curve1d units2) (Curve1d units3) where
  Zero / _ = Zero
  Constant x / Constant y = Constant (x / y)
  curve / Constant x = Units.add ((1.0 / Units.drop x) * Units.drop curve)
  curve1 / curve2 = Quotient curve1 curve2

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Curve1d units1) (Qty units2) (Curve1d units3) where
  curve / value = curve / constant value

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Curve1d units2) (Curve1d units3) where
  value / curve = constant value / curve

squared :: Squared (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
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

sqrt :: Squared (Qty units1) (Qty units2) => Curve1d units2 -> Curve1d units1
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
isZero curve = List.all [pointOn curve t ~= Qty.zero | t <- Quadrature.points]

maxRootOrder :: Int
maxRootOrder = 4

----- ROOT FINDING -----

data IsZero = IsZero deriving (Eq, Show)

roots :: Tolerance units => Curve1d units -> Result IsZero (List Root)
roots Zero = Err IsZero
roots (Constant value) = if value ~= Qty.zero then Err IsZero else Ok []
roots curve | isZero curve = Err IsZero
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
solve curveDerivative derivativeOrder region@(Region domain nonZeroDerivativeOrder nonZeroDerivativeSign)
  | derivativeOrder == nonZeroDerivativeOrder = [NonZero domain nonZeroDerivativeSign]
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
                 in if pointOn ?originalCurve rootX ~= Qty.zero
                      then
                        let root = Root rootX derivativeOrder nextDerivativeSign
                            width = computeWidth (derivativeOrder + 1) (pointOn nextDerivative rootX)
                         in [Solution root width]
                      else
                        [ NonZero (Range.from x1 rootX) -nextDerivativeSign
                        , NonZero (Range.from rootX x2) nextDerivativeSign
                        ]
           where
            Range x1 x2 = subdomain
            minX = if nextDerivativeSign == Positive then x1 else x2
            maxX = if nextDerivativeSign == Positive then x2 else x1
            minY = pointOn curveDerivative minX
            maxY = pointOn curveDerivative maxX

          -- Check if a high-order root should in fact be a lower-order root (e.g. in y=x^3+x
          -- the 3rd derivative is zero at x=0 but it is in fact a 0th-order root, not a
          -- 2nd-order root)
          lift (Solution currentRoot currentWidth) =
            let rootX = Root.value currentRoot
                rootY = pointOn curveDerivative rootX
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
          let midY = pointOn curve midX
           in if midY >= Qty.zero
                then bisectMonotonic curve lowX midX lowY midY
                else bisectMonotonic curve midX highX midY highY

regions :: Tolerance units => Range Unitless -> Curve1d units -> Result IsZero (List Region)
regions domain curve =
  case resolve domain curve of
    Just region -> Ok [region]
    Nothing -> do
      (leftDomain, rightDomain) <- bisect domain
      leftRegions <- regions leftDomain curve
      rightRegions <- regions rightDomain curve
      Ok (leftRegions ++ rightRegions)

bisect :: Range Unitless -> Result IsZero (Range Unitless, Range Unitless)
bisect domain = if Range.isAtomic domain then Err IsZero else Ok (Range.bisect domain)

resolve :: Tolerance units => Range Unitless -> Curve1d units -> Maybe Region
resolve domain curve
  | minValue >= ?tolerance = Just (Region domain 0 Positive)
  | maxValue <= negate ?tolerance = Just (Region domain 0 Negative)
  | otherwise = resolveDerivative domain (derivative curve) 1
 where
  Range minValue maxValue = segmentBounds curve domain

minDerivativeResolution :: Float
minDerivativeResolution = 0.5

resolveDerivative :: Range Unitless -> Curve1d units -> Int -> Maybe Region
resolveDerivative domain curveDerivative derivativeOrder
  | derivativeResolution >= minDerivativeResolution = Just (Region domain derivativeOrder Positive)
  | derivativeResolution <= -minDerivativeResolution = Just (Region domain derivativeOrder Negative)
  | derivativeOrder <= maxRootOrder = resolveDerivative domain (derivative curveDerivative) (derivativeOrder + 1)
  | otherwise = Nothing
 where
  derivativeResolution = resolution domain curveDerivative

resolution :: Range Unitless -> Curve1d units -> Float
resolution domain curveDerivative
  | minValue > Qty.zero = minValue / maxValue
  | maxValue < Qty.zero = negate (maxValue / minValue)
  | otherwise = 0.0
 where
  Range minValue maxValue = segmentBounds curveDerivative domain

solveEndpoint :: Tolerance units => Curve1d units -> Float -> (List Root, Float)
solveEndpoint curve endpointX | not (pointOn curve endpointX ~= Qty.zero) = ([], endpointX)
solveEndpoint curve endpointX = check (derivative curve) 1 Qty.infinity Nothing
 where
  check curveDerivative derivativeOrder currentMinWidth currentBest =
    let derivativeValue = pointOn curveDerivative endpointX
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

resolveEndpoint :: Root -> Curve1d units -> Float -> Float -> (List Root, Float)
resolveEndpoint root curveDerivative endpointX innerX =
  let domain = Range.from endpointX innerX
   in if Qty.abs (resolution domain curveDerivative) >= minDerivativeResolution
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
  Float.pow (factorial derivativeOrder * ?tolerance / Qty.abs derivativeValue) (1.0 / float derivativeOrder)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
