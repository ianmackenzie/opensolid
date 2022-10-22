module Range (
    Range,
    constant,
    from,
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

import OpenSolid hiding (abs, sqrt)
import qualified OpenSolid
import qualified Quantity
import Range.Unsafe
import qualified Units

constant :: Quantity units -> Range units
constant value =
    Range value value

from :: Quantity units -> Quantity units -> Range units
from a b =
    Range (min a b) (max a b)

minValue :: Range units -> Quantity units
minValue range =
    let (Range low _) = range in low

maxValue :: Range units -> Quantity units
maxValue range =
    let (Range _ high) = range in high

isAtomic :: Range units -> Bool
isAtomic range =
    let (Range low high) = range
        mid = midpoint range
     in mid == low || mid == high

midpoint :: Range units -> Quantity units
midpoint range =
    let (Range low high) = range
     in Quantity.midpoint low high

endpoints :: Range units -> (Quantity units, Quantity units)
endpoints (Range low high) =
    (low, high)

squared :: Units.Multiplication units units => Range units -> Range (Units.Product units units)
squared range
    | low >= Quantity.zero = Range ll hh
    | high <= Quantity.zero = Range hh ll
    | otherwise = Range Quantity.zero (max ll hh)
  where
    (Range low high) = range
    ll = low * low
    hh = high * high

sqrt :: Units.Sqrt units => Range units -> Range (Units.SquareRoot units)
sqrt range =
    let (Range low high) = range
        sqrtLow = OpenSolid.sqrt (max low Quantity.zero)
        sqrtHigh = OpenSolid.sqrt (max high Quantity.zero)
     in Range sqrtLow sqrtHigh

contains :: Quantity units -> Range units -> Bool
contains quantity range =
    let (Range low high) = range
     in low <= quantity && quantity <= high

bisect :: Range units -> (Range units, Range units)
bisect range =
    let (Range low high) = range
        mid = midpoint range
     in (Range low mid, Range mid high)

abs :: Range units -> Range units
abs range
    | low >= Quantity.zero = range
    | high <= Quantity.zero = - range
    | otherwise = Range Quantity.zero (max high (negate low))
  where
    (Range low high) = range
