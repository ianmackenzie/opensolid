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

data Curve1d units = forall curve. IsCurve1d curve units => Curve1d curve

instance IsCurve1d (Curve1d units) units where
    pointOn (Curve1d curve) t =
        pointOn curve t

    segmentBounds (Curve1d curve) t =
        segmentBounds curve t

    derivative (Curve1d curve) =
        derivative curve

instance Units.Coercion (Curve1d units) (Curve1d Unitless)

zero :: Curve1d units
zero =
    constant Qty.zero

newtype Constant units = Constant (Qty units)

instance IsCurve1d (Constant units) units where
    pointOn (Constant value) _ =
        value

    segmentBounds (Constant value) _ =
        Range.constant value

    derivative (Constant _) =
        zero

constant :: Qty units -> Curve1d units
constant value =
    Curve1d (Constant value)

data Parameter = Parameter

instance IsCurve1d Parameter Unitless where
    pointOn Parameter t = t
    segmentBounds Parameter t = t
    derivative Parameter = constant 1.0

parameter :: Curve1d Unitless
parameter =
    Curve1d Parameter

newtype Negated units = Negated (Curve1d units)

instance IsCurve1d (Negated units) units where
    pointOn (Negated curve) t =
        negate (pointOn curve t)

    segmentBounds (Negated curve) t =
        negate (segmentBounds curve t)

    derivative (Negated curve) =
        negate (derivative curve)

instance Negation (Curve1d units) where
    negate curve =
        Curve1d (Negated curve)

data Sum units = Sum (Curve1d units) (Curve1d units)

instance IsCurve1d (Sum units) units where
    pointOn (Sum curve1 curve2) t =
        pointOn curve1 t + pointOn curve2 t

    segmentBounds (Sum curve1 curve2) t =
        segmentBounds curve1 t + segmentBounds curve2 t

    derivative (Sum curve1 curve2) =
        derivative curve1 + derivative curve2

instance Addition Curve1d Curve1d Curve1d where
    curve1 + curve2 =
        Curve1d (Sum curve1 curve2)

instance Addition Curve1d Qty Curve1d where
    curve + value =
        curve + constant value

instance Addition Qty Curve1d Curve1d where
    value + curve =
        constant value + curve

data Difference units = Difference (Curve1d units) (Curve1d units)

instance IsCurve1d (Difference units) units where
    pointOn (Difference curve1 curve2) t =
        pointOn curve1 t - pointOn curve2 t

    segmentBounds (Difference curve1 curve2) t =
        segmentBounds curve1 t - segmentBounds curve2 t

    derivative (Difference curve1 curve2) =
        derivative curve1 - derivative curve2

instance Subtraction Curve1d Curve1d Curve1d where
    curve1 - curve2 =
        Curve1d (Difference curve1 curve2)

instance Subtraction Curve1d Qty Curve1d where
    curve - value =
        curve - constant value

instance Subtraction Qty Curve1d Curve1d where
    value - curve =
        constant value - curve

data Product units1 units2 = Product (Curve1d units1) (Curve1d units2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => IsCurve1d (Product units1 units2) units3 where
    pointOn (Product curve1 curve2) t =
        pointOn curve1 t * pointOn curve2 t

    segmentBounds (Product curve1 curve2) t =
        segmentBounds curve1 t * segmentBounds curve2 t

    derivative (Product curve1 curve2) =
        derivative curve1 * curve2 + curve1 * derivative curve2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3) where
    curve1 * curve2 =
        Curve1d (Product curve1 curve2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Qty units2) (Curve1d units3) where
    curve * value =
        curve * constant value

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Curve1d units2) (Curve1d units3) where
    value * curve =
        constant value * curve

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Vector3d units2 coordinates) (VectorCurve3d units3 coordinates) where
    curve * vector =
        curve * VectorCurve3d.constant vector

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector3d units1 coordinates) (Curve1d units2) (VectorCurve3d units3 coordinates) where
    vector * curve =
        VectorCurve3d.constant vector * curve

data Quotient units1 units2 = Quotient (Curve1d units1) (Curve1d units2)

instance Division (Qty units1) (Qty units2) (Qty units3) => IsCurve1d (Quotient units1 units2) units3 where
    pointOn (Quotient curve1 curve2) t =
        pointOn curve1 t / pointOn curve2 t

    segmentBounds (Quotient curve1 curve2) t =
        segmentBounds curve1 t / segmentBounds curve2 t

    derivative (Quotient curve1 curve2) =
        let p = Units.drop curve1
            q = Units.drop curve2
            p' = derivative p
            q' = derivative q
         in Units.add ((p' * q - p * q') / squared q)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Curve1d units1) (Curve1d units2) (Curve1d units3) where
    curve1 / curve2 =
        Curve1d (Quotient curve1 curve2)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Curve1d units1) (Qty units2) (Curve1d units3) where
    curve / value =
        curve / constant value

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Curve1d units2) (Curve1d units3) where
    value / curve =
        constant value / curve

newtype Squared units = Squared (Curve1d units)

instance Multiplication (Qty units1) (Qty units1) (Qty units2) => IsCurve1d (Squared units1) units2 where
    pointOn (Squared curve) t =
        let x = pointOn curve t in x * x

    segmentBounds (Squared curve) t =
        let x = segmentBounds curve t in Range.squared x

    derivative (Squared curve) =
        2.0 * Curve1d curve * derivative curve

squared :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
squared curve =
    Curve1d (Squared curve)

newtype SquareRoot units = SquareRoot (Curve1d units)

instance Sqrt (Qty units1) (Qty units2) => IsCurve1d (SquareRoot units1) units2 where
    pointOn (SquareRoot curve) t =
        Qty.sqrt (pointOn curve t)

    segmentBounds (SquareRoot curve) t =
        Range.sqrt (segmentBounds curve t)

    derivative (SquareRoot curve) =
        let f = Units.drop curve
         in Units.add (derivative f / (2.0 * sqrt f))

sqrt :: Sqrt (Qty units1) (Qty units2) => Curve1d units1 -> Curve1d units2
sqrt curve =
    Curve1d (SquareRoot curve)

newtype Sin = Sin (Curve1d Radians)

instance IsCurve1d Sin Unitless where
    pointOn (Sin curve) t =
        Qty.sin (pointOn curve t)

    segmentBounds (Sin curve) t =
        Range.sin (segmentBounds curve t)

    derivative (Sin curve) =
        cos curve * Units.drop (derivative curve)

sin :: Curve1d Radians -> Curve1d Unitless
sin curve =
    Curve1d (Sin curve)

newtype Cos = Cos (Curve1d Radians)

instance IsCurve1d Cos Unitless where
    pointOn (Cos curve) t =
        Qty.cos (pointOn curve t)

    segmentBounds (Cos curve) t =
        Range.cos (segmentBounds curve t)

    derivative (Cos curve) =
        negate (sin curve) * Units.drop (derivative curve)

cos :: Curve1d Radians -> Curve1d Unitless
cos curve =
    Curve1d (Cos curve)

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
