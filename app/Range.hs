module Range (
    Range,
    constant,
    from,
    unit,
    minValue,
    maxValue,
    isAtomic,
    midpoint,
    endpoints,
    squared,
    contains,
    bisect,
    abs,
    sqrt,
) where

import OpenSolid
import Range.Unsafe
import qualified Scalar

constant :: Scalar scalar => scalar -> Range scalar
constant value =
    Range value value

from :: Scalar scalar => scalar -> scalar -> Range scalar
from a b =
    Range (min a b) (max a b)

unit :: Range Float
unit =
    Range 0.0 1.0

minValue :: Scalar scalar => Range scalar -> scalar
minValue range =
    let (Range low _) = range in low

maxValue :: Scalar scalar => Range scalar -> scalar
maxValue range =
    let (Range _ high) = range in high

isAtomic :: Scalar scalar => Range scalar -> Bool
isAtomic range =
    let (Range low high) = range
        mid = midpoint range
     in mid == low || mid == high

midpoint :: Scalar scalar => Range scalar -> scalar
midpoint range =
    let (Range low high) = range
     in Scalar.midpoint low high

endpoints :: Scalar scalar => Range scalar -> (scalar, scalar)
endpoints (Range low high) =
    (low, high)

squared :: (Scalar scalar, Scalar squaredScalar, Multiplication scalar scalar squaredScalar) => Range scalar -> Range squaredScalar
squared range
    | low >= Scalar.zero = Range ll hh
    | high <= Scalar.zero = Range hh ll
    | otherwise = Range Scalar.zero (max ll hh)
  where
    (Range low high) = range
    ll = low * low
    hh = high * high

sqrt :: (Scalar scalar, Scalar sqrtScalar, Sqrt scalar sqrtScalar) => Range scalar -> Range sqrtScalar
sqrt range =
    let (Range low high) = range
        sqrtLow = Scalar.sqrt (max low Scalar.zero)
        sqrtHigh = Scalar.sqrt (max high Scalar.zero)
     in Range sqrtLow sqrtHigh

contains :: Scalar scalar => scalar -> Range scalar -> Bool
contains scalar range =
    let (Range low high) = range
     in low <= scalar && scalar <= high

bisect :: Scalar scalar => Range scalar -> (Range scalar, Range scalar)
bisect range =
    let (Range low high) = range
        mid = midpoint range
     in (Range low mid, Range mid high)

abs :: Scalar scalar => Range scalar -> Range scalar
abs range
    | low >= Scalar.zero = range
    | high <= Scalar.zero = negate range
    | otherwise = Range Scalar.zero (max high (negate low))
  where
    (Range low high) = range
