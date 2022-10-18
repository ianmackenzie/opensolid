module Expression1d (
    Expression1d (Expression1d),
    evaluate,
    bounds,
    derivative,
    zero,
    constant,
    parameter,
    squared,
    roots,
) where

import Expression1d.Root (Root (..))
import qualified Expression1d.Root as Root
import Interval (Interval)
import qualified Interval
import qualified List
import OpenSolid
import qualified Quantity
import Range (Range)
import qualified Range
import Range.Unsafe
import qualified Units

data Expression1d units = Expression1d
    { evaluate :: !(Float -> Quantity units)
    , bounds :: !(Interval -> Range units)
    , derivative :: ~(Expression1d units)
    }

zero :: Expression1d units
zero =
    constant Quantity.zero

constant :: Quantity units -> Expression1d units
constant value =
    Expression1d
        { evaluate = always value
        , bounds = always (Range.constant value)
        , derivative = constant Quantity.zero
        }

parameter :: Expression1d Unitless
parameter =
    Expression1d
        { evaluate = identity
        , bounds = identity
        , derivative = constant 1.0
        }

instance Negation (Expression1d units) where
    negate expression =
        Expression1d
            { evaluate = evaluate expression >>> negate
            , bounds = bounds expression >>> negate
            , derivative = negate (derivative expression)
            }

instance Addition Expression1d Quantity where
    type Sum Expression1d Quantity = Expression1d
    expression + quantity =
        Expression1d
            { evaluate = evaluate expression >>> (+ quantity)
            , bounds = bounds expression >>> (+ quantity)
            , derivative = derivative expression
            }

instance Addition Quantity Expression1d where
    type Sum Quantity Expression1d = Expression1d
    quantity + expression =
        expression + quantity

instance Addition Expression1d Expression1d where
    type Sum Expression1d Expression1d = Expression1d
    expression1 + expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t + evaluate expression2 t
            , bounds = \t -> bounds expression1 t + bounds expression2 t
            , derivative = derivative expression1 + derivative expression2
            }

instance Subtraction Expression1d Quantity where
    type Difference Expression1d Quantity = Expression1d
    expression - quantity =
        Expression1d
            { evaluate = evaluate expression >>> subtract quantity
            , bounds = bounds expression >>> subtract quantity
            , derivative = derivative expression
            }

instance Subtraction Quantity Expression1d where
    type Difference Quantity Expression1d = Expression1d
    quantity - expression =
        Expression1d
            { evaluate = (quantity -) <<< evaluate expression
            , bounds = (quantity -) <<< bounds expression
            , derivative = negate (derivative expression)
            }

instance Subtraction Expression1d Expression1d where
    type Difference Expression1d Expression1d = Expression1d
    expression1 - expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t - evaluate expression2 t
            , bounds = \t -> bounds expression1 t - bounds expression2 t
            , derivative = derivative expression1 - derivative expression2
            }

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Expression1d units2) where
    type Product (Quantity units1) (Expression1d units2) = Expression1d (Units.Product units1 units2)
    quantity * expression =
        Expression1d
            { evaluate = (quantity *) <<< evaluate expression
            , bounds = (quantity *) <<< bounds expression
            , derivative = quantity * derivative expression
            }

instance Units.Multiplication units1 units2 => Multiplication (Expression1d units1) (Quantity units2) where
    type Product (Expression1d units1) (Quantity units2) = Expression1d (Units.Product units1 units2)
    expression * quantity =
        Expression1d
            { evaluate = evaluate expression >>> (* quantity)
            , bounds = bounds expression >>> (* quantity)
            , derivative = derivative expression * quantity
            }

instance Units.Multiplication units1 units2 => Multiplication (Expression1d units1) (Expression1d units2) where
    type Product (Expression1d units1) (Expression1d units2) = Expression1d (Units.Product units1 units2)
    expression1 * expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t * evaluate expression2 t
            , bounds = \t -> bounds expression1 t * bounds expression2 t
            , derivative = derivative expression1 * expression2 + expression1 * derivative expression2
            }

squared :: Units.Multiplication units units => Expression1d units -> Expression1d (Units.Product units units)
squared expression =
    Expression1d
        { evaluate = evaluate expression >>> (\value -> value * value)
        , bounds = bounds expression >>> Range.squared
        , derivative = 2.0 * expression * derivative expression
        }

data Neighborhood
    = HasRoot !Interval !Root
    | NoRoot !Interval

data Indeterminate = Indeterminate

maxRootOrder :: Int
maxRootOrder =
    8

roots :: Quantity units -> Expression1d units -> List Root
roots tolerance expression =
    case neighborhoods expression tolerance Interval.unit of
        Ok neighborhoods -> List.collect neighborhoodRoot neighborhoods
        Err Indeterminate -> []

neighborhoods :: Expression1d units -> Quantity units -> Interval -> Result Indeterminate (List Neighborhood)
neighborhoods expression tolerance domain =
    case localNeighborhoods expression tolerance domain 0 of
        Ok neighborhoods -> Ok neighborhoods
        Err Indeterminate ->
            if Interval.isAtomic domain
                then Err Indeterminate
                else do
                    let (leftDomain, rightDomain) = Interval.bisect domain
                    leftNeighborhoods <- neighborhoods expression tolerance leftDomain
                    rightNeighborhoods <- neighborhoods expression tolerance rightDomain
                    Ok (leftNeighborhoods ++ rightNeighborhoods)

localNeighborhoods :: Expression1d units -> Quantity units -> Interval -> Int -> Result Indeterminate (List Neighborhood)
localNeighborhoods expression tolerance domain order
    | definitelyNonZero expression tolerance domain = Ok [NoRoot domain]
    | order <= maxRootOrder = do
        derivativeNeighborhoods <- localNeighborhoods (derivative expression) tolerance domain (order + 1)
        Ok (List.combine (solve expression tolerance order) derivativeNeighborhoods)
    -- We've passed the maximum root order and still haven't found a non-zero derivative
    | otherwise = Err Indeterminate

definitelyNonZero :: Expression1d units -> Quantity units -> Interval -> Bool
definitelyNonZero expression tolerance domain =
    let yRange = bounds expression domain
     in Range.minValue yRange >= tolerance || Range.maxValue yRange <= - tolerance

solve :: Expression1d units -> Quantity units -> Int -> Neighborhood -> List Neighborhood
solve expression tolerance order derivativeNeighborhood =
    case derivativeNeighborhood of
        NoRoot domain ->
            case solveMonotonic expression domain of
                Just x -> [HasRoot domain Root{value = x, order = order}]
                Nothing -> [NoRoot domain]
        HasRoot domain root ->
            let rootX = Root.value root
             in if abs (evaluate expression rootX) <= tolerance
                    then [HasRoot domain root]
                    else
                        let (x1, x2) = Interval.endpoints domain
                            leftDomain = Interval.from x1 rootX
                            rightDomain = Interval.from rootX x2
                            leftNeighborhoods = solve expression tolerance order (NoRoot leftDomain)
                            rightNeighborhoods = solve expression tolerance order (NoRoot rightDomain)
                         in leftNeighborhoods ++ rightNeighborhoods

solveMonotonic :: Expression1d units -> Interval -> Maybe Float
solveMonotonic expression domain
    | y1 <= Quantity.zero && y2 >= Quantity.zero = Just (bisectMonotonic expression x1 x2)
    | y1 >= Quantity.zero && y2 <= Quantity.zero = Just (bisectMonotonic expression x2 x1)
    | otherwise = Nothing
  where
    (x1, x2) = Interval.endpoints domain
    y1 = evaluate expression x1
    y2 = evaluate expression x2

bisectMonotonic :: Expression1d units -> Float -> Float -> Float
bisectMonotonic expression lowX highX =
    let midX = Quantity.midpoint lowX highX
     in if midX == lowX || midX == highX
            then midX
            else
                let midY = evaluate expression midX
                 in if midY >= Quantity.zero
                        then bisectMonotonic expression lowX midX
                        else bisectMonotonic expression midX highX

neighborhoodRoot :: Neighborhood -> Maybe Root
neighborhoodRoot neighborhood =
    case neighborhood of
        HasRoot _ root -> Just root
        NoRoot _ -> Nothing
