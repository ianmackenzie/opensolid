module Curve1d (
    Curve1d (Curve1d),
    IsCurve1d,
    pointOn,
    segmentBounds,
    derivative,
    zero,
    constant,
    parameter,
    squared,
    sqrt,
    sin,
    cos,
    roots,
) where

import Angle qualified
import Generic qualified
import List qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Units qualified
import Vector3d (Vector3d)
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
    Squared :: forall units1 units2. Multiplication (Qty units1) (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
    SquareRoot :: forall units1 units2. Sqrt (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
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
            Quotient c1 c2 ->
                let p = Units.drop c1
                    q = Units.drop c2
                    d = (derivative p * q - p * derivative q) / squared q
                 in Units.add d
            Squared c -> 2.0 * c * derivative c
            SquareRoot c -> let f = Units.drop c in Units.add (derivative f / (2.0 * sqrt f))
            Sin c -> cos c * Units.drop (derivative c)
            Cos c -> negate (sin c) * Units.drop (derivative c)

zero :: Curve1d units
zero = Zero

constant :: Qty units -> Curve1d units
constant value = if value == Qty.zero then Zero else Constant value

parameter :: Curve1d Unitless
parameter = Parameter

instance Generic.Zero Curve1d where
    zero = Zero

instance Negation (Curve1d units) where
    negate Zero = Zero
    negate (Constant x) = Constant (negate x)
    negate (Negated curve) = curve
    negate (Product c1 c2) = negate c1 * c2
    negate curve = Negated curve

instance Addition Curve1d Curve1d Curve1d where
    Zero + curve = curve
    curve + Zero = curve
    Constant x + Constant y = constant (x + y)
    curve1 + curve2 = Sum curve1 curve2

instance Addition Curve1d Qty Curve1d where
    curve + value = curve + constant value

instance Addition Qty Curve1d Curve1d where
    value + curve = constant value + curve

instance Subtraction Curve1d Curve1d Curve1d where
    Zero - curve = negate curve
    curve - Zero = curve
    Constant x - Constant y = constant (x - y)
    curve1 - curve2 = Difference curve1 curve2

instance Subtraction Curve1d Qty Curve1d where
    curve - value = curve - constant value

instance Subtraction Qty Curve1d Curve1d where
    value - curve = constant value - curve

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3) where
    Zero * _ = Zero
    _ * Zero = Zero
    Constant x * Constant y = Constant (x * y)
    Constant x * curve | Units.drop x == 1.0 = Units.add (Units.drop curve)
    Constant x * curve | Units.drop x == -1.0 = Units.add (Units.drop (negate curve))
    Constant x * Negated c = negate x * c
    c1 * c2@(Constant _) = Units.add (Units.drop c2 * Units.drop c1)
    Constant x * Product (Constant y) c = Units.add (Product (Constant (Units.drop x * Units.drop y)) (Units.drop c))
    curve1 * curve2 = Product curve1 curve2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Qty units2) (Curve1d units3) where
    curve * value = curve * constant value

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Curve1d units2) (Curve1d units3) where
    value * curve = constant value * curve

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

squared :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
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

sqrt :: Sqrt (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
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

roots :: Qty units -> Curve1d units -> List Float
roots tolerance curve =
    let firstDerivative = derivative curve
        secondDerivative = derivative firstDerivative
        root0 = [0.0 | Qty.abs (pointOn curve 0.0) <= tolerance]
        root1 = [1.0 | Qty.abs (pointOn curve 1.0) <= tolerance]
     in deduplicate (root0 ++ solve tolerance curve firstDerivative secondDerivative Range.unit ++ root1)

deduplicate :: List Float -> List Float
deduplicate list = List.foldr prependRoot [] list

prependRoot :: Float -> List Float -> List Float
prependRoot root list = if List.head list == Just root then list else root : list

solve :: Qty units -> Curve1d units -> Curve1d units -> Curve1d units -> Range Unitless -> List Float
solve tolerance curve firstDerivative secondDerivative domain
    | nonZero curve tolerance domain = []
    | resolution firstDerivative domain > 0.5 = List.compact [solveMonotonic curve domain]
    | resolution secondDerivative domain > 0.5 =
        case solveMonotonic firstDerivative domain of
            Just rootX ->
                if Qty.abs (pointOn curve rootX) <= tolerance
                    then [rootX]
                    else
                        let (x1, x2) = Range.endpoints domain
                            leftRoot = solveMonotonic curve (Range.from x1 rootX)
                            rightRoot = solveMonotonic curve (Range.from rootX x2)
                         in List.compact [leftRoot, rightRoot]
            Nothing -> List.compact [solveMonotonic curve domain]
    | Range.isAtomic domain = []
    | otherwise =
        let (leftDomain, rightDomain) = Range.bisect domain
            leftRoots = solve tolerance curve firstDerivative secondDerivative leftDomain
            rightRoots = solve tolerance curve firstDerivative secondDerivative rightDomain
         in leftRoots ++ rightRoots

nonZero :: Curve1d units -> Qty units -> Range Unitless -> Bool
nonZero curve tolerance domain =
    let range = segmentBounds curve domain
     in Range.minValue range >= tolerance || Range.maxValue range <= negate tolerance

resolution :: Curve1d units -> Range Unitless -> Float
resolution curve domain =
    let absMagnitude = Range.abs (segmentBounds curve domain)
        minMagnitude = Range.minValue absMagnitude
        maxMagnitude = Range.maxValue absMagnitude
     in if maxMagnitude == Qty.zero then 0.0 else minMagnitude / maxMagnitude

solveMonotonic :: Curve1d units -> Range Unitless -> Maybe Float
solveMonotonic curve domain
    | y1 <= Qty.zero && y2 >= Qty.zero = Just (bisectMonotonic curve x1 x2)
    | y1 >= Qty.zero && y2 <= Qty.zero = Just (bisectMonotonic curve x2 x1)
    | otherwise = Nothing
  where
    (x1, x2) = Range.endpoints domain
    y1 = pointOn curve x1
    y2 = pointOn curve x2

bisectMonotonic :: Curve1d units -> Float -> Float -> Float
bisectMonotonic curve lowX highX =
    let midX = Qty.midpoint lowX highX
     in if midX == lowX || midX == highX
            then midX
            else
                let midY = pointOn curve midX
                 in if midY >= Qty.zero
                        then bisectMonotonic curve lowX midX
                        else bisectMonotonic curve midX highX
