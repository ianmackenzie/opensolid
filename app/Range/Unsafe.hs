module Range.Unsafe (Range (..)) where

import Bounds
import OpenSolid
import qualified Scalar
import qualified Units

data Range scalar = Range !scalar !scalar
    deriving (Eq, Show)

instance Units.Coercion (Range scalar) (Range Float)

instance Scalar scalar => Negation (Range scalar) where
    negate (Range low high) =
        Range (negate high) (negate low)

instance Scalar scalar => Addition (Range scalar) where
    (Range low1 high1) + (Range low2 high2) =
        Range (low1 + low2) (high1 + high2)

instance Scalar scalar => Subtraction (Range scalar) where
    (Range low1 high1) - (Range low2 high2) =
        Range (low1 - high2) (high1 - low2)

scalarRangeMultiplication :: (Scalar scalar1, Scalar scalar2, Scalar scalarProduct, Multiplication scalar1 scalar2 scalarProduct) => scalar1 -> Range scalar2 -> Range scalarProduct
scalarRangeMultiplication value (Range low high) =
    if value >= Scalar.zero
        then Range (value * low) (value * high)
        else Range (value * high) (value * low)

rangeScalarMultiplication :: (Scalar scalar1, Scalar scalar2, Scalar scalarProduct, Multiplication scalar2 scalar1 scalarProduct) => Range scalar1 -> scalar2 -> Range scalarProduct
rangeScalarMultiplication =
    flip scalarRangeMultiplication

scalarRangeDivision :: (Scalar scalar1, Scalar scalar2, Scalar scalarQuotient, Division scalar1 scalar2 scalarQuotient) => scalar1 -> Range scalar2 -> Range scalarQuotient
scalarRangeDivision n (Range dl dh)
    | dl > Scalar.zero || dh < Scalar.zero = Range (n / dh) (n / dl)
    | otherwise = Range Scalar.negativeInfinity Scalar.positiveInfinity

rangeScalarDivision :: (Scalar scalar1, Scalar scalar2, Scalar scalarQuotient, Division scalar1 scalar2 scalarQuotient) => Range scalar1 -> scalar2 -> Range scalarQuotient
rangeScalarDivision (Range nl nh) d
    | d > Scalar.zero = Range (nl / d) (nh / d)
    | d < Scalar.zero = Range (nh / d) (nl / d)
    | otherwise = Range Scalar.negativeInfinity Scalar.positiveInfinity

instance Scalar scalar => Multiplication Float (Range scalar) (Range scalar) where
    (*) = scalarRangeMultiplication

instance Scalar scalar => Multiplication (Range scalar) Float (Range scalar) where
    (*) = rangeScalarMultiplication

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Quantity units) (Range scalar) (Range result) where
    (*) = scalarRangeMultiplication

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Range scalar) (Quantity units) (Range result) where
    (*) = rangeScalarMultiplication

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => Multiplication (Range scalar1) (Range scalar2) (Range result) where
    (Range low1 high1) * (Range low2 high2) =
        let ll = low1 * low2
            lh = low1 * high2
            hl = high1 * low2
            hh = high1 * high2
            low = min (min (min ll lh) hl) hh
            high = max (max (max ll lh) hl) hh
         in Range low high

instance (Scalar scalar, Scalar result, Division Float scalar result) => Division Float (Range scalar) (Range result) where
    (/) = scalarRangeDivision

instance (Scalar scalar, Scalar result, Division (Quantity units) scalar result) => Division (Quantity units) (Range scalar) (Range result) where
    (/) = scalarRangeDivision

instance Scalar scalar => Division (Range scalar) Float (Range scalar) where
    (/) = rangeScalarDivision

instance (Scalar scalar, Scalar result, Division scalar (Quantity units) result) => Division (Range scalar) (Quantity units) (Range result) where
    (/) = rangeScalarDivision

instance (Scalar scalar1, Scalar scalar2, Scalar result, Division scalar1 scalar2 result) => Division (Range scalar1) (Range scalar2) (Range result) where
    (Range nl nh) / (Range dl dh)
        | dl > Scalar.zero = Range (nl / dh) (nh / dl)
        | dh < Scalar.zero = Range (nh / dh) (nl / dl)
        | otherwise = Range Scalar.negativeInfinity Scalar.positiveInfinity

instance Scalar scalar => Bounds (Range scalar) where
    aggregate (Range low1 high1) (Range low2 high2) =
        Range (min low1 low2) (max high1 high2)

    overlaps (Range low1 high1) (Range low2 high2) =
        high1 >= low2 && low1 <= high2
