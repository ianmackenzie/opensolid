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

import Curve1d.Root (Root (Root))
import qualified Curve1d.Root as Root
import qualified List
import OpenSolid hiding (cos, sin, sqrt, zero)
import qualified Qty
import qualified Range
import Range.Unsafe
import qualified Result
import qualified Units
import Vector3d (Vector3d)
import {-# SOURCE #-} VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} qualified VectorCurve3d

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
            Sin c -> Qty.sin (pointOn c t)
            Cos c -> Qty.cos (pointOn c t)

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
zero =
    Zero

constant :: Qty units -> Curve1d units
constant value =
    if value == Qty.zero then Zero else Constant value

parameter :: Curve1d Unitless
parameter =
    Parameter

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
squared curve =
    case curve of
        Zero -> Zero
        Constant x -> Constant (x * x)
        Negated c -> squared c
        Cos c -> Units.add (cosSquared c)
        Sin c -> Units.add (sinSquared c)
        _ -> Squared curve

cosSquared :: Curve1d Radians -> Curve1d Unitless
cosSquared c =
    0.5 * cos (2.0 * c) + 0.5

sinSquared :: Curve1d Radians -> Curve1d Unitless
sinSquared c =
    0.5 - 0.5 * cos (2.0 * c)

sqrt :: Sqrt (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
sqrt curve =
    case curve of
        Zero -> Zero
        Constant x -> Constant (Qty.sqrt x)
        _ -> SquareRoot curve

sin :: Curve1d Radians -> Curve1d Unitless
sin curve =
    Sin curve

cos :: Curve1d Radians -> Curve1d Unitless
cos curve =
    Cos curve

data Neighborhood
    = HasRoot !(Range Unitless) !Root
    | NoRoot !(Range Unitless)
    deriving (Show)

data Indeterminate = Indeterminate deriving (Show)

maxRootOrder :: Int
maxRootOrder =
    8

roots :: Qty units -> Curve1d units -> List Root
roots tolerance curve =
    case neighborhoods curve tolerance Range.unit of
        Ok validNeighborhoods -> List.collect neighborhoodRoot validNeighborhoods
        Err Indeterminate -> []

neighborhoods :: Curve1d units -> Qty units -> Range Unitless -> Result Indeterminate (List Neighborhood)
neighborhoods curve tolerance domain =
    case localNeighborhoods curve tolerance domain 0 of
        Ok validNeighborhoods -> Ok validNeighborhoods
        Err Indeterminate ->
            if Range.isAtomic domain
                then Err Indeterminate
                else Result.do
                    let (leftDomain, rightDomain) = Range.bisect domain
                    leftNeighborhoods <- neighborhoods curve tolerance leftDomain
                    rightNeighborhoods <- neighborhoods curve tolerance rightDomain
                    Ok (leftNeighborhoods ++ rightNeighborhoods)

localNeighborhoods :: Curve1d units -> Qty units -> Range Unitless -> Int -> Result Indeterminate (List Neighborhood)
localNeighborhoods curve tolerance domain order
    | definitelyNonZero curve tolerance domain = Ok [NoRoot domain]
    | order <= maxRootOrder = Result.do
        derivativeNeighborhoods <- localNeighborhoods (derivative curve) tolerance domain (order + 1)
        Ok (List.combine (solve curve tolerance order) derivativeNeighborhoods)
    -- We've passed the maximum root order and still haven't found a non-zero derivative
    | otherwise = Err Indeterminate

definitelyNonZero :: Curve1d units -> Qty units -> Range Unitless -> Bool
definitelyNonZero curve tolerance domain =
    let yRange = segmentBounds curve domain
     in Range.minValue yRange >= tolerance || Range.maxValue yRange <= negate tolerance

solve :: Curve1d units -> Qty units -> Int -> Neighborhood -> List Neighborhood
solve curve tolerance order derivativeNeighborhood =
    case derivativeNeighborhood of
        NoRoot domain -> [solveMonotonic curve order domain]
        HasRoot domain root ->
            let rootX = Root.value root
             in if abs (pointOn curve rootX) <= tolerance
                    then [HasRoot domain root]
                    else
                        let (x1, x2) = Range.endpoints domain
                            leftDomain = Range.from x1 rootX
                            rightDomain = Range.from rootX x2
                            leftNeighborhoods = if x1 < rootX then solve curve tolerance order (NoRoot leftDomain) else []
                            rightNeighborhoods = if rootX < x2 then solve curve tolerance order (NoRoot rightDomain) else []
                         in leftNeighborhoods ++ rightNeighborhoods

solveMonotonic :: Curve1d units -> Int -> Range Unitless -> Neighborhood
solveMonotonic curve order domain
    | y1 <= Qty.zero && y2 >= Qty.zero = HasRoot domain Root{Root.value = bisectMonotonic curve x1 x2, Root.order = order, Root.sign = Root.Positive}
    | y1 >= Qty.zero && y2 <= Qty.zero = HasRoot domain Root{Root.value = bisectMonotonic curve x2 x1, Root.order = order, Root.sign = Root.Negative}
    | otherwise = NoRoot domain
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

neighborhoodRoot :: Neighborhood -> Maybe Root
neighborhoodRoot neighborhood =
    case neighborhood of
        HasRoot _ root -> Just root
        NoRoot _ -> Nothing
