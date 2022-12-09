module Range (
    Range,
    unsafe,
    constant,
    from,
    unit,
    minValue,
    maxValue,
    isAtomic,
    midpoint,
    endpoints,
    width,
    squared,
    contains,
    bisect,
    abs,
    sqrt,
    aggregate,
    overlaps,
    sin,
    cos,
) where

import Angle qualified
import Bounds (Bounds (..))
import OpenSolid hiding (abs, cos, sin, sqrt, tan)
import Qty qualified
import Units qualified

data Range units = Range !(Qty units) !(Qty units)
    deriving (Eq)

deriving instance Show (Qty units) => Show (Range units)

instance Units.Coercion (Range units) (Range Unitless)

instance Negation (Range units) where
    negate (Range low high) = Range (negate high) (negate low)

instance Addition Range Range Range where
    (Range low1 high1) + (Range low2 high2) = Range (low1 + low2) (high1 + high2)

instance Addition Range Qty Range where
    (Range low high) + value = Range (low + value) (high + value)

instance Addition Qty Range Range where
    value + (Range low high) = Range (value + low) (value + high)

instance Subtraction Range Range Range where
    (Range low1 high1) - (Range low2 high2) = Range (low1 - high2) (high1 - low2)

instance Subtraction Range Qty Range where
    (Range low high) - value = Range (low - value) (high - value)

instance Subtraction Qty Range Range where
    value - (Range low high) = Range (value - high) (value - low)

qtyRangeMultiplication :: Multiplication (Qty units1) (Qty units2) (Qty units3) => Qty units1 -> Range units2 -> Range units3
qtyRangeMultiplication value (Range low high)
    | value >= zero = Range (value * low) (value * high)
    | otherwise = Range (value * high) (value * low)

rangeQtyMultiplication :: Multiplication (Qty units1) (Qty units2) (Qty units3) => Range units1 -> Qty units2 -> Range units3
rangeQtyMultiplication (Range low high) value
    | value >= zero = Range (low * value) (high * value)
    | otherwise = Range (high * value) (low * value)

qtyRangeDivision :: Division (Qty units1) (Qty units2) (Qty units3) => Qty units1 -> Range units2 -> Range units3
qtyRangeDivision n (Range dl dh)
    | dl > zero || dh < zero = Range (n / dh) (n / dl)
    | otherwise = Range -infinity infinity

rangeQtyDivision :: Division (Qty units1) (Qty units2) (Qty units3) => Range units1 -> Qty units2 -> Range units3
rangeQtyDivision (Range nl nh) d
    | d > zero = Range (nl / d) (nh / d)
    | d < zero = Range (nh / d) (nl / d)
    | otherwise = Range -infinity infinity

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Range units2) (Range units3) where
    (*) = qtyRangeMultiplication

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Range units1) (Qty units2) (Range units3) where
    (*) = rangeQtyMultiplication

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Range units1) (Range units2) (Range units3) where
    (Range low1 high1) * (Range low2 high2) =
        let ll = low1 * low2
            lh = low1 * high2
            hl = high1 * low2
            hh = high1 * high2
            low = min (min (min ll lh) hl) hh
            high = max (max (max ll lh) hl) hh
         in Range low high

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Range units2) (Range units3) where
    (/) = qtyRangeDivision

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Range units1) (Qty units2) (Range units3) where
    (/) = rangeQtyDivision

instance (Division (Qty units1) (Qty units2) (Qty units3)) => Division (Range units1) (Range units2) (Range units3) where
    (Range nl nh) / (Range dl dh)
        | dl > zero = Range (nl / dh) (nh / dl)
        | dh < zero = Range (nh / dh) (nl / dl)
        | otherwise = Range -infinity infinity

instance Bounds (Range units) where
    aggregate (Range low1 high1) (Range low2 high2) = Range (min low1 low2) (max high1 high2)

    overlaps (Range low1 high1) (Range low2 high2) = low1 <= high2 && low2 <= high1

unsafe :: Qty units -> Qty units -> Range units
unsafe = Range

constant :: Qty units -> Range units
constant value = Range value value

from :: Qty units -> Qty units -> Range units
from a b = Range (min a b) (max a b)

unit :: Range Unitless
unit = Range 0.0 1.0

minValue :: Range units -> Qty units
minValue (Range low _) = low

maxValue :: Range units -> Qty units
maxValue (Range _ high) = high

isAtomic :: Range units -> Bool
isAtomic range =
    let (Range low high) = range
        mid = midpoint range
     in mid == low || mid == high

midpoint :: Range units -> Qty units
midpoint (Range low high) = Qty.midpoint low high

endpoints :: Range units -> (Qty units, Qty units)
endpoints (Range low high) = (low, high)

width :: Range units -> Qty units
width (Range low high) = high - low

squared :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Range units1 -> Range units2
squared (Range low high)
    | low >= zero = Range ll hh
    | high <= zero = Range hh ll
    | otherwise = Range zero (max ll hh)
  where
    ll = low * low
    hh = high * high

sqrt :: Sqrt (Qty units1) (Qty units2) => Range units1 -> Range units2
sqrt (Range low high) =
    Range (Qty.sqrt (max low zero)) (Qty.sqrt (max high zero))

contains :: Qty units -> Range units -> Bool
contains value (Range low high) = low <= value && value <= high

bisect :: Range units -> (Range units, Range units)
bisect range =
    let (Range low high) = range
        mid = midpoint range
     in (Range low mid, Range mid high)

abs :: Range units -> Range units
abs range
    | low >= zero = range
    | high <= zero = -range
    | otherwise = Range zero (max high -low)
  where
    (Range low high) = range

sin :: Range Radians -> Range Unitless
sin range =
    let (Range low high) = range
        (includesMin, includesMax) = sinIncludesMinMax range
        newLow = if includesMin then -1.0 else min (Qty.sin low) (Qty.sin high)
        newHigh = if includesMax then 1.0 else max (Qty.sin low) (Qty.sin high)
     in Range newLow newHigh

cos :: Range Radians -> Range Unitless
cos range =
    let (Range low high) = range
        (includesMin, includesMax) = cosIncludesMinMax range
        newLow = if includesMin then -1.0 else min (Qty.cos low) (Qty.cos high)
        newHigh = if includesMax then 1.0 else max (Qty.cos low) (Qty.cos high)
     in Range newLow newHigh

sinIncludesMinMax :: Range Radians -> (Bool, Bool)
sinIncludesMinMax range = cosIncludesMinMax (range - Angle.radians (pi / 2))

cosIncludesMinMax :: Range Radians -> (Bool, Bool)
cosIncludesMinMax interval = (cosIncludesMax (interval + Angle.radians pi), cosIncludesMax interval)

cosIncludesMax :: Range Radians -> Bool
cosIncludesMax (Range low high) =
    let twoPi = Angle.radians (2 * pi)
     in floor (low / twoPi) /= floor (high / twoPi)
