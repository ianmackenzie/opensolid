module Interval (
    Interval,
    singleton,
    from,
    lowerBound,
    upperBound,
    endpoints,
    squared,
) where

import Interval.Unsafe
import OpenSolid
import qualified Quantity
import qualified String
import qualified Units

instance Negation (Interval units) where
    negate (Interval low high) =
        Interval (- high) (- low)

instance Addition Interval Quantity where
    type Sum Interval Quantity = Interval
    (Interval low high) + quantity =
        Interval (low + quantity) (high + quantity)

instance Addition Quantity Interval where
    type Sum Quantity Interval = Interval
    quantity + (Interval low high) =
        Interval (quantity + low) (quantity + high)

instance Addition Interval Interval where
    type Sum Interval Interval = Interval
    (Interval low1 high1) + (Interval low2 high2) =
        Interval (low1 + low2) (high1 + high2)

instance Subtraction Interval Quantity where
    type Difference Interval Quantity = Interval
    (Interval low high) - quantity =
        Interval (low - quantity) (high - quantity)

instance Subtraction Quantity Interval where
    type Difference Quantity Interval = Interval
    quantity - (Interval low high) =
        Interval (quantity - high) (quantity - low)

instance Subtraction Interval Interval where
    type Difference Interval Interval = Interval
    (Interval low1 high1) - (Interval low2 high2) =
        Interval (low1 - high2) (high1 - low2)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Interval units2) where
    type Product (Quantity units1) (Interval units2) = Interval (Units.Product units1 units2)
    quantity * (Interval low high) =
        if quantity >= Quantity.zero
            then Interval (quantity * low) (quantity * high)
            else Interval (quantity * high) (quantity * low)

instance Units.Multiplication units1 units2 => Multiplication (Interval units1) (Quantity units2) where
    type Product (Interval units1) (Quantity units2) = Interval (Units.Product units1 units2)
    (Interval low high) * quantity =
        if quantity >= Quantity.zero
            then Interval (low * quantity) (high * quantity)
            else Interval (high * quantity) (low * quantity)

instance Units.Multiplication units1 units2 => Multiplication (Interval units1) (Interval units2) where
    type Product (Interval units1) (Interval units2) = Interval (Units.Product units1 units2)
    (Interval low1 high1) * (Interval low2 high2) =
        let ll = low1 * low2
            lh = low1 * high2
            hl = high1 * low2
            hh = high1 * high2
            low = min (min (min ll lh) hl) hh
            high = max (max (max ll lh) hl) hh
         in Interval low high

singleton :: Quantity units -> Interval units
singleton value =
    Interval value value

from :: Quantity units -> Quantity units -> Interval units
from a b =
    Interval (min a b) (max a b)

lowerBound :: Interval units -> Quantity units
lowerBound interval =
    let (Interval low _) = interval in low

upperBound :: Interval units -> Quantity units
upperBound interval =
    let (Interval _ high) = interval in high

endpoints :: Interval units -> (Quantity units, Quantity units)
endpoints (Interval low high) =
    (low, high)

squared :: Units.Multiplication units units => Interval units -> Interval (Units.Product units units)
squared (Interval low high)
    | low >= Quantity.zero = Interval ll hh
    | high <= Quantity.zero = Interval hh ll
    | otherwise = Interval Quantity.zero (max ll hh)
  where
    ll = low * low
    hh = high * high
