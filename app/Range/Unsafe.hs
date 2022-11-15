module Range.Unsafe (Range (..)) where

import Bounds
import OpenSolid
import qualified Qty
import qualified Units

data Range qty = Range !qty !qty
    deriving (Eq, Show)

instance Units.Coercion (Range (Qty a)) (Range Float)

instance Negation (Range (Qty a)) where
    negate (Range low high) =
        Range (negate high) (negate low)

instance Addition Range Range Range (Qty a) where
    (Range low1 high1) + (Range low2 high2) =
        Range (low1 + low2) (high1 + high2)

instance Subtraction Range Range Range (Qty a) where
    (Range low1 high1) - (Range low2 high2) =
        Range (low1 - high2) (high1 - low2)

qtyRangeMultiplication :: Multiplication (Qty a) (Qty b) (Qty c) => Qty a -> Range (Qty b) -> Range (Qty c)
qtyRangeMultiplication value (Range low high) =
    if value >= Qty.zero
        then Range (value * low) (value * high)
        else Range (value * high) (value * low)

rangeQtyMultiplication :: Multiplication (Qty a) (Qty b) (Qty c) => Range (Qty a) -> Qty b -> Range (Qty c)
rangeQtyMultiplication (Range low high) value =
    if value >= Qty.zero
        then Range (low * value) (high * value)
        else Range (high * value) (low * value)

qtyRangeDivision :: Division (Qty a) (Qty b) (Qty c) => Qty a -> Range (Qty b) -> Range (Qty c)
qtyRangeDivision n (Range dl dh)
    | dl > Qty.zero || dh < Qty.zero = Range (n / dh) (n / dl)
    | otherwise = Range Qty.negativeInfinity Qty.positiveInfinity

rangeQtyDivision :: Division (Qty a) (Qty b) (Qty c) => Range (Qty a) -> Qty b -> Range (Qty c)
rangeQtyDivision (Range nl nh) d
    | d > Qty.zero = Range (nl / d) (nh / d)
    | d < Qty.zero = Range (nh / d) (nl / d)
    | otherwise = Range Qty.negativeInfinity Qty.positiveInfinity

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Qty a) (Range (Qty b)) (Range (Qty c)) where
    (*) = qtyRangeMultiplication

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Range (Qty a)) (Qty b) (Range (Qty c)) where
    (*) = rangeQtyMultiplication

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Range (Qty a)) (Range (Qty b)) (Range (Qty c)) where
    (Range low1 high1) * (Range low2 high2) =
        let ll = low1 * low2
            lh = low1 * high2
            hl = high1 * low2
            hh = high1 * high2
            low = min (min (min ll lh) hl) hh
            high = max (max (max ll lh) hl) hh
         in Range low high

instance Division (Qty a) (Qty b) (Qty c) => Division (Qty a) (Range (Qty b)) (Range (Qty c)) where
    (/) = qtyRangeDivision

instance Division (Qty a) (Qty b) (Qty c) => Division (Range (Qty a)) (Qty b) (Range (Qty c)) where
    (/) = rangeQtyDivision

instance (Division (Qty a) (Qty b) (Qty c)) => Division (Range (Qty a)) (Range (Qty b)) (Range (Qty c)) where
    (Range nl nh) / (Range dl dh)
        | dl > Qty.zero = Range (nl / dh) (nh / dl)
        | dh < Qty.zero = Range (nh / dh) (nl / dl)
        | otherwise = Range Qty.negativeInfinity Qty.positiveInfinity

instance Bounds (Range (Qty a)) where
    aggregate (Range low1 high1) (Range low2 high2) =
        Range (min low1 low2) (max high1 high2)

    overlaps (Range low1 high1) (Range low2 high2) =
        high1 >= low2 && low1 <= high2
