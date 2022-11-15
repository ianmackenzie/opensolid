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
import OpenSolid hiding (abs, sqrt)
import qualified Qty
import Range.Unsafe

constant :: Qty units -> Range units
constant value =
    Range value value

from :: Qty units -> Qty units -> Range units
from a b =
    Range (min a b) (max a b)

unit :: Range Unitless
unit =
    Range 0.0 1.0

minValue :: Range units -> Qty units
minValue range =
    let (Range low _) = range in low

maxValue :: Range units -> Qty units
maxValue range =
    let (Range _ high) = range in high

isAtomic :: Range units -> Bool
isAtomic range =
    let (Range low high) = range
        mid = midpoint range
     in mid == low || mid == high

midpoint :: Range units -> Qty units
midpoint range =
    let (Range low high) = range
     in Qty.midpoint low high

endpoints :: Range units -> (Qty units, Qty units)
endpoints (Range low high) =
    (low, high)

squared :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Range units1 -> Range units2
squared range
    | low >= zero = Range ll hh
    | high <= zero = Range hh ll
    | otherwise = Range zero (max ll hh)
  where
    (Range low high) = range
    ll = low * low
    hh = high * high

sqrt :: Sqrt (Qty units1) (Qty units2) => Range units1 -> Range units2
sqrt range =
    let (Range low high) = range
        sqrtLow = Qty.sqrt (max low zero)
        sqrtHigh = Qty.sqrt (max high zero)
     in Range sqrtLow sqrtHigh

contains :: Qty units -> Range units -> Bool
contains value range =
    let (Range low high) = range
     in low <= value && value <= high

bisect :: Range units -> (Range units, Range units)
bisect range =
    let (Range low high) = range
        mid = midpoint range
     in (Range low mid, Range mid high)

abs :: Range units -> Range units
abs range
    | low >= zero = range
    | high <= zero = negate range
    | otherwise = Range zero (max high (negate low))
  where
    (Range low high) = range

aggregate :: Range units -> Range units -> Range units
aggregate =
    Bounds.aggregate

overlaps :: Range units -> Range units -> Bool
overlaps =
    Bounds.overlaps
