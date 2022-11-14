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
    aggregate,
    overlaps,
) where

import qualified Bounds
import OpenSolid
import qualified Qty
import Range.Unsafe

constant :: Qty a -> Range (Qty a)
constant value =
    Range value value

from :: Qty a -> Qty a -> Range (Qty a)
from a b =
    Range (min a b) (max a b)

unit :: Range Float
unit =
    Range 0.0 1.0

minValue :: Range (Qty a) -> Qty a
minValue range =
    let (Range low _) = range in low

maxValue :: Range (Qty a) -> Qty a
maxValue range =
    let (Range _ high) = range in high

isAtomic :: Range (Qty a) -> Bool
isAtomic range =
    let (Range low high) = range
        mid = midpoint range
     in mid == low || mid == high

midpoint :: Range (Qty a) -> Qty a
midpoint range =
    let (Range low high) = range
     in Qty.midpoint low high

endpoints :: Range (Qty a) -> (Qty a, Qty a)
endpoints (Range low high) =
    (low, high)

squared :: Multiplication (Qty a) (Qty a) (Qty b) => Range (Qty a) -> Range (Qty b)
squared range
    | low >= Qty.zero = Range ll hh
    | high <= Qty.zero = Range hh ll
    | otherwise = Range Qty.zero (max ll hh)
  where
    (Range low high) = range
    ll = low * low
    hh = high * high

sqrt :: Sqrt (Qty a) (Qty b) => Range (Qty a) -> Range (Qty b)
sqrt range =
    let (Range low high) = range
        sqrtLow = Qty.sqrt (max low Qty.zero)
        sqrtHigh = Qty.sqrt (max high Qty.zero)
     in Range sqrtLow sqrtHigh

contains :: Qty a -> Range (Qty a) -> Bool
contains value range =
    let (Range low high) = range
     in low <= value && value <= high

bisect :: Range (Qty a) -> (Range (Qty a), Range (Qty a))
bisect range =
    let (Range low high) = range
        mid = midpoint range
     in (Range low mid, Range mid high)

abs :: Range (Qty a) -> Range (Qty a)
abs range
    | low >= Qty.zero = range
    | high <= Qty.zero = negate range
    | otherwise = Range Qty.zero (max high (negate low))
  where
    (Range low high) = range

aggregate :: Range (Qty a) -> Range (Qty a) -> Range (Qty a)
aggregate =
    Bounds.aggregate

overlaps :: Range (Qty a) -> Range (Qty a) -> Bool
overlaps =
    Bounds.overlaps
