module Range.Unsafe (Range (..)) where

import Bounds
import OpenSolid
import qualified Units

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
    | otherwise = Range (-infinity) infinity

rangeQtyDivision :: Division (Qty units1) (Qty units2) (Qty units3) => Range units1 -> Qty units2 -> Range units3
rangeQtyDivision (Range nl nh) d
    | d > zero = Range (nl / d) (nh / d)
    | d < zero = Range (nh / d) (nl / d)
    | otherwise = Range (-infinity) infinity

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
        | otherwise = Range (-infinity) infinity

instance Bounds (Range units) where
    aggregate (Range low1 high1) (Range low2 high2) = Range (min low1 low2) (max high1 high2)

    overlaps (Range low1 high1) (Range low2 high2) = low1 <= high2 && low2 <= high1
