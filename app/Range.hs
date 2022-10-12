module Range (
    Range,
    constant,
    from,
    minValue,
    maxValue,
    midpoint,
    endpoints,
    squared,
    contains,
    bisect,
    abs,
) where

import OpenSolid hiding (abs)
import qualified OpenSolid
import qualified Quantity
import Range.Unsafe
import qualified String
import qualified Units

instance Negation (Range units) where
    negate (Range low high) =
        Range (- high) (- low)

instance Addition Range Quantity where
    type Sum Range Quantity = Range
    (Range low high) + quantity =
        Range (low + quantity) (high + quantity)

instance Addition Quantity Range where
    type Sum Quantity Range = Range
    quantity + (Range low high) =
        Range (quantity + low) (quantity + high)

instance Addition Range Range where
    type Sum Range Range = Range
    (Range low1 high1) + (Range low2 high2) =
        Range (low1 + low2) (high1 + high2)

instance Subtraction Range Quantity where
    type Difference Range Quantity = Range
    (Range low high) - quantity =
        Range (low - quantity) (high - quantity)

instance Subtraction Quantity Range where
    type Difference Quantity Range = Range
    quantity - (Range low high) =
        Range (quantity - high) (quantity - low)

instance Subtraction Range Range where
    type Difference Range Range = Range
    (Range low1 high1) - (Range low2 high2) =
        Range (low1 - high2) (high1 - low2)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Range units2) where
    type Product (Quantity units1) (Range units2) = Range (Units.Product units1 units2)
    quantity * (Range low high) =
        if quantity >= Quantity.zero
            then Range (quantity * low) (quantity * high)
            else Range (quantity * high) (quantity * low)

instance Units.Multiplication units1 units2 => Multiplication (Range units1) (Quantity units2) where
    type Product (Range units1) (Quantity units2) = Range (Units.Product units1 units2)
    (Range low high) * quantity =
        if quantity >= Quantity.zero
            then Range (low * quantity) (high * quantity)
            else Range (high * quantity) (low * quantity)

instance Units.Multiplication units1 units2 => Multiplication (Range units1) (Range units2) where
    type Product (Range units1) (Range units2) = Range (Units.Product units1 units2)
    (Range low1 high1) * (Range low2 high2) =
        let ll = low1 * low2
            lh = low1 * high2
            hl = high1 * low2
            hh = high1 * high2
            low = min (min (min ll lh) hl) hh
            high = max (max (max ll lh) hl) hh
         in Range low high

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
    | otherwise = Range Quantity.zero (max high (- low))
  where
    (Range low high) = range
