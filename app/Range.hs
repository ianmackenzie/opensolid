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
    interpolate,
    interpolationParameter,
) where

import Angle qualified
import Bounds (Bounds (..))
import Float qualified
import Generic qualified
import OpenSolid
import Qty qualified
import Units qualified

data Range units = Range !(Qty units) !(Qty units)
    deriving (Eq)

deriving instance Show (Qty units) => Show (Range units)

instance Units.Coercion (Range units) (Range Unitless)

instance Negation (Range units) where
    negate (Range low high) = Range (negate high) (negate low)

instance Generic.Zero Range where
    zero = constant Qty.zero

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
    | value >= Qty.zero = Range (value * low) (value * high)
    | otherwise = Range (value * high) (value * low)

rangeQtyMultiplication :: Multiplication (Qty units1) (Qty units2) (Qty units3) => Range units1 -> Qty units2 -> Range units3
rangeQtyMultiplication (Range low high) value
    | value >= Qty.zero = Range (low * value) (high * value)
    | otherwise = Range (high * value) (low * value)

qtyRangeDivision :: Division (Qty units1) (Qty units2) (Qty units3) => Qty units1 -> Range units2 -> Range units3
qtyRangeDivision n (Range dl dh)
    | dl > Qty.zero || dh < Qty.zero = Range (n / dh) (n / dl)
    | otherwise = Range -Qty.infinity Qty.infinity

rangeQtyDivision :: Division (Qty units1) (Qty units2) (Qty units3) => Range units1 -> Qty units2 -> Range units3
rangeQtyDivision (Range nl nh) d
    | d > Qty.zero = Range (nl / d) (nh / d)
    | d < Qty.zero = Range (nh / d) (nl / d)
    | otherwise = Range -Qty.infinity Qty.infinity

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
        | dl > Qty.zero = Range (nl / dh) (nh / dl)
        | dh < Qty.zero = Range (nh / dh) (nl / dl)
        | otherwise = Range -Qty.infinity Qty.infinity

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

squared :: Squared (Qty units1) (Qty units2) => Range units1 -> Range units2
squared (Range low high)
    | low >= Qty.zero = Range ll hh
    | high <= Qty.zero = Range hh ll
    | otherwise = Range Qty.zero (max ll hh)
  where
    ll = low * low
    hh = high * high

sqrt :: Squared (Qty units1) (Qty units2) => Range units2 -> Range units1
sqrt (Range low high) =
    Range (Qty.sqrt (max low Qty.zero)) (Qty.sqrt (max high Qty.zero))

contains :: Qty units -> Range units -> Bool
contains value (Range low high) = low <= value && value <= high

bisect :: Range units -> (Range units, Range units)
bisect range =
    let (Range low high) = range
        mid = midpoint range
     in (Range low mid, Range mid high)

abs :: Range units -> Range units
abs range
    | low >= Qty.zero = range
    | high <= Qty.zero = -range
    | otherwise = Range Qty.zero (max high -low)
  where
    (Range low high) = range

sin :: Range Radians -> Range Unitless
sin range =
    let (Range low high) = range
        (includesMin, includesMax) = sinIncludesMinMax range
        newLow = if includesMin then -1.0 else min (Angle.sin low) (Angle.sin high)
        newHigh = if includesMax then 1.0 else max (Angle.sin low) (Angle.sin high)
     in Range newLow newHigh

cos :: Range Radians -> Range Unitless
cos range =
    let (Range low high) = range
        (includesMin, includesMax) = cosIncludesMinMax range
        newLow = if includesMin then -1.0 else min (Angle.cos low) (Angle.cos high)
        newHigh = if includesMax then 1.0 else max (Angle.cos low) (Angle.cos high)
     in Range newLow newHigh

sinIncludesMinMax :: Range Radians -> (Bool, Bool)
sinIncludesMinMax range = cosIncludesMinMax (range - Angle.radians (Float.pi / 2))

cosIncludesMinMax :: Range Radians -> (Bool, Bool)
cosIncludesMinMax interval =
    ( cosIncludesMax (interval + Angle.radians Float.pi)
    , cosIncludesMax interval
    )

cosIncludesMax :: Range Radians -> Bool
cosIncludesMax (Range low high) =
    let twoPi = Angle.radians (2 * Float.pi)
     in Float.floor (low / twoPi) /= Float.floor (high / twoPi)

interpolate :: Range units -> Float -> Qty units
interpolate (Range low high) t =
    Qty.interpolateFrom low high t

interpolationParameter :: Range units -> Qty units -> Float
interpolationParameter (Range low high) value
    | low < high = (value - low) / (high - low)
    | value < low = -Qty.infinity
    | value > high = Qty.infinity
    | otherwise = 0.0
