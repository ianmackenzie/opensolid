module Range.Unsafe (Range (..)) where

import OpenSolid
import qualified Quantity
import qualified Units

data Range units = Range !(Quantity units) !(Quantity units)
    deriving (Eq)

deriving instance Show (Quantity units) => Show (Range units)

instance Negation (Range units) where
    negate (Range low high) =
        Range (negate high) (negate low)

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

instance Units.Division units1 units2 => Division (Range units1) (Quantity units2) where
    type Quotient (Range units1) (Quantity units2) = Range (Units.Quotient units1 units2)
    (Range nl nh) / d
        | d > Quantity.zero = Range (nl / d) (nh / d)
        | d < Quantity.zero = Range (nh / d) (nl / d)
        | otherwise = Range Quantity.negativeInfinity Quantity.positiveInfinity

instance Units.Division units1 units2 => Division (Quantity units1) (Range units2) where
    type Quotient (Quantity units1) (Range units2) = Range (Units.Quotient units1 units2)
    n / (Range dl dh)
        | dl > Quantity.zero || dh < Quantity.zero = Range (n / dh) (n / dl)
        | otherwise = Range Quantity.negativeInfinity Quantity.positiveInfinity

instance Units.Division units1 units2 => Division (Range units1) (Range units2) where
    type Quotient (Range units1) (Range units2) = Range (Units.Quotient units1 units2)
    (Range nl nh) / (Range dl dh)
        | dl > Quantity.zero = Range (nl / dh) (nh / dl)
        | dh < Quantity.zero = Range (nh / dh) (nl / dl)
        | otherwise = Range Quantity.negativeInfinity Quantity.positiveInfinity
