module Expression1d (
    Expression1d (Expression1d),
    evaluate,
    bounds,
    derivative,
    zero,
    constant,
    parameter,
    squared,
    sqrt,
    roots,
) where

import Expression1d.Root (Root (Root))
import qualified Expression1d.Root as Root
import qualified List
import OpenSolid
import qualified Range
import Range.Unsafe
import qualified Scalar
import qualified Units

data Expression1d scalar = Expression1d
    { evaluate :: !(Float -> scalar)
    , bounds :: !(Range Float -> Range scalar)
    , derivative :: ~(Expression1d scalar)
    }

instance Units.Coercion (Expression1d scalar) (Expression1d Float)

zero :: Scalar scalar => Expression1d scalar
zero =
    constant Scalar.zero

constant :: Scalar scalar => scalar -> Expression1d scalar
constant value =
    Expression1d
        { evaluate = always value
        , bounds = always (Range.constant value)
        , derivative = constant Scalar.zero
        }

parameter :: Expression1d Float
parameter =
    Expression1d
        { evaluate = identity
        , bounds = identity
        , derivative = constant 1.0
        }

instance Scalar scalar => Negation (Expression1d scalar) where
    negate expression =
        Expression1d
            { evaluate = evaluate expression >>> negate
            , bounds = bounds expression >>> negate
            , derivative = negate (derivative expression)
            }

instance Scalar scalar => Addition (Expression1d scalar) where
    expression1 + expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t + evaluate expression2 t
            , bounds = \t -> bounds expression1 t + bounds expression2 t
            , derivative = derivative expression1 + derivative expression2
            }

instance Scalar scalar => Subtraction (Expression1d scalar) where
    expression1 - expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t - evaluate expression2 t
            , bounds = \t -> bounds expression1 t - bounds expression2 t
            , derivative = derivative expression1 - derivative expression2
            }

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => Multiplication (Expression1d scalar1) (Expression1d scalar2) (Expression1d result) where
    expression1 * expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t * evaluate expression2 t
            , bounds = \t -> bounds expression1 t * bounds expression2 t
            , derivative = derivative expression1 * expression2 + expression1 * derivative expression2
            }

instance (Scalar scalar1, Scalar scalar2, Scalar result, Division scalar1 scalar2 result) => Division (Expression1d scalar1) (Expression1d scalar2) (Expression1d result) where
    expression1 / expression2 =
        Expression1d
            { evaluate = \t -> evaluate expression1 t / evaluate expression2 t
            , bounds = \t -> bounds expression1 t / bounds expression2 t
            , derivative =
                let p = Units.drop expression1
                    q = Units.drop expression2
                    p' = derivative p
                    q' = derivative q
                 in Units.add ((p' * q - p * q') / squared q)
            }

squared :: (Scalar scalar, Scalar squaredScalar, Multiplication scalar scalar squaredScalar) => Expression1d scalar -> Expression1d squaredScalar
squared expression =
    Expression1d
        { evaluate = evaluate expression >>> (\value -> value * value)
        , bounds = bounds expression >>> Range.squared
        , derivative = constant 2.0 * expression * derivative expression
        }

sqrt :: Sqrt scalar sqrtQuantity => Expression1d scalar -> Expression1d sqrtQuantity
sqrt expression =
    Expression1d
        { evaluate = evaluate expression >>> Scalar.sqrt
        , bounds = bounds expression >>> Range.sqrt
        , derivative =
            let f = Units.drop expression :: Expression1d Float
             in Units.add (derivative f / (constant 2.0 * sqrt f))
        }

data Neighborhood
    = HasRoot !(Range Float) !Root
    | NoRoot !(Range Float)

data Indeterminate = Indeterminate

maxRootOrder :: Int
maxRootOrder =
    8

roots :: Scalar scalar => scalar -> Expression1d scalar -> List Root
roots tolerance expression =
    case neighborhoods expression tolerance Range.unit of
        Ok validNeighborhoods -> List.collect neighborhoodRoot validNeighborhoods
        Err Indeterminate -> []

neighborhoods :: Scalar scalar => Expression1d scalar -> scalar -> Range Float -> Result Indeterminate (List Neighborhood)
neighborhoods expression tolerance domain =
    case localNeighborhoods expression tolerance domain 0 of
        Ok validNeighborhoods -> Ok validNeighborhoods
        Err Indeterminate ->
            if Range.isAtomic domain
                then Err Indeterminate
                else do
                    let (leftDomain, rightDomain) = Range.bisect domain
                    leftNeighborhoods <- neighborhoods expression tolerance leftDomain
                    rightNeighborhoods <- neighborhoods expression tolerance rightDomain
                    Ok (leftNeighborhoods ++ rightNeighborhoods)

localNeighborhoods :: Scalar scalar => Expression1d scalar -> scalar -> Range Float -> Int -> Result Indeterminate (List Neighborhood)
localNeighborhoods expression tolerance domain order
    | definitelyNonZero expression tolerance domain = Ok [NoRoot domain]
    | order <= maxRootOrder = do
        derivativeNeighborhoods <- localNeighborhoods (derivative expression) tolerance domain (order + 1)
        Ok (List.combine (solve expression tolerance order) derivativeNeighborhoods)
    -- We've passed the maximum root order and still haven't found a non-zero derivative
    | otherwise = Err Indeterminate

definitelyNonZero :: Scalar scalar => Expression1d scalar -> scalar -> Range Float -> Bool
definitelyNonZero expression tolerance domain =
    let yRange = bounds expression domain
     in Range.minValue yRange >= tolerance || Range.maxValue yRange <= negate tolerance

solve :: Scalar scalar => Expression1d scalar -> scalar -> Int -> Neighborhood -> List Neighborhood
solve expression tolerance order derivativeNeighborhood =
    case derivativeNeighborhood of
        NoRoot domain ->
            case solveMonotonic expression domain of
                Just x -> [HasRoot domain Root{Root.value = x, Root.order = order}]
                Nothing -> [NoRoot domain]
        HasRoot domain root ->
            let rootX = Root.value root
             in if Scalar.abs (evaluate expression rootX) <= tolerance
                    then [HasRoot domain root]
                    else
                        let (x1, x2) = Range.endpoints domain
                            leftDomain = Range.from x1 rootX
                            rightDomain = Range.from rootX x2
                            leftNeighborhoods = solve expression tolerance order (NoRoot leftDomain)
                            rightNeighborhoods = solve expression tolerance order (NoRoot rightDomain)
                         in leftNeighborhoods ++ rightNeighborhoods

solveMonotonic :: Scalar scalar => Expression1d scalar -> Range Float -> Maybe Float
solveMonotonic expression domain
    | y1 <= Scalar.zero && y2 >= Scalar.zero = Just (bisectMonotonic expression x1 x2)
    | y1 >= Scalar.zero && y2 <= Scalar.zero = Just (bisectMonotonic expression x2 x1)
    | otherwise = Nothing
  where
    (x1, x2) = Range.endpoints domain
    y1 = evaluate expression x1
    y2 = evaluate expression x2

bisectMonotonic :: Scalar scalar => Expression1d scalar -> Float -> Float -> Float
bisectMonotonic expression lowX highX =
    let midX = Scalar.midpoint lowX highX
     in if midX == lowX || midX == highX
            then midX
            else
                let midY = evaluate expression midX
                 in if midY >= Scalar.zero
                        then bisectMonotonic expression lowX midX
                        else bisectMonotonic expression midX highX

neighborhoodRoot :: Neighborhood -> Maybe Root
neighborhoodRoot neighborhood =
    case neighborhood of
        HasRoot _ root -> Just root
        NoRoot _ -> Nothing
