module Range.Unsafe (Range (..)) where

import OpenSolid
import qualified Quantity
import qualified Units

data Range quantity = Range !quantity !quantity
    deriving (Eq, Show)

instance Quantity quantity => Negation (Range quantity) where
    negate (Range low high) =
        Range (negate high) (negate low)

instance Quantity quantity => Addition (Range quantity) where
    (Range low1 high1) + (Range low2 high2) =
        Range (low1 + low2) (high1 + high2)

instance Quantity quantity => Subtraction (Range quantity) where
    (Range low1 high1) - (Range low2 high2) =
        Range (low1 - high2) (high1 - low2)

instance {-# INCOHERENT #-} Multiplication quantity1 quantity2 result => Multiplication quantity1 (Range quantity2) (Range result) where
    quantity * (Range low high) =
        if quantity >= Quantity.zero
            then Range (quantity * low) (quantity * high)
            else Range (quantity * high) (quantity * low)

instance {-# INCOHERENT #-} Multiplication quantity1 quantity2 result => Multiplication (Range quantity1) quantity2 (Range result) where
    (Range low high) * quantity =
        if quantity >= Quantity.zero
            then Range (low * quantity) (high * quantity)
            else Range (high * quantity) (low * quantity)

instance {-# INCOHERENT #-} Multiplication quantity1 quantity2 result => Multiplication (Range quantity1) (Range quantity2) (Range result) where
    (Range low1 high1) * (Range low2 high2) =
        let ll = low1 * low2
            lh = low1 * high2
            hl = high1 * low2
            hh = high1 * high2
            low = min (min (min ll lh) hl) hh
            high = max (max (max ll lh) hl) hh
         in Range low high

instance {-# INCOHERENT #-} (Quantity quantity1, Quantity quantity2, Division quantity1 quantity2 result) => Division (Range quantity1) quantity2 (Range result) where
    (Range nl nh) / d
        | d > Quantity.zero = Range (nl / d) (nh / d)
        | d < Quantity.zero = Range (nh / d) (nl / d)
        | otherwise = Range Quantity.negativeInfinity Quantity.positiveInfinity

instance {-# INCOHERENT #-} (Quantity quantity1, Quantity quantity2, Division quantity1 quantity2 result) => Division quantity1 (Range quantity2) (Range result) where
    n / (Range dl dh)
        | dl > Quantity.zero || dh < Quantity.zero = Range (n / dh) (n / dl)
        | otherwise = Range Quantity.negativeInfinity Quantity.positiveInfinity

instance {-# INCOHERENT #-} (Quantity quantity1, Quantity quantity2, Division quantity1 quantity2 result) => Division (Range quantity1) (Range quantity2) (Range result) where
    (Range nl nh) / (Range dl dh)
        | dl > Quantity.zero = Range (nl / dh) (nh / dl)
        | dh < Quantity.zero = Range (nh / dh) (nl / dl)
        | otherwise = Range Quantity.negativeInfinity Quantity.positiveInfinity
