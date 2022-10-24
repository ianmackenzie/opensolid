module Scalar (
    zero,
    positiveInfinity,
    negativeInfinity,
    infinity,
    isNaN,
    interpolateFrom,
    midpoint,
    Sqrt,
    sqrt,
    abs,
    clamp,
) where

import Data.Coerce (coerce)
import OpenSolid
import qualified Prelude

zero :: Scalar scalar => scalar
zero =
    coerce 0.0

positiveInfinity :: Scalar scalar => scalar
positiveInfinity =
    coerce (1.0 / 0.0)

negativeInfinity :: Scalar scalar => scalar
negativeInfinity =
    negate positiveInfinity

infinity :: Scalar scalar => scalar
infinity =
    positiveInfinity

isNaN :: Scalar scalar => scalar -> Bool
isNaN value =
    Prelude.isNaN (coerce value :: Float)

interpolateFrom :: Scalar scalar => scalar -> scalar -> Float -> scalar
interpolateFrom a b t =
    if t <= 0.5
        then a + (b - a) * t
        else b + (a - b) * (1.0 - t)

midpoint :: Scalar scalar => scalar -> scalar -> scalar
midpoint a b =
    0.5 * (a + b)

abs :: Scalar scalar => scalar -> scalar
abs value =
    coerce (Prelude.abs (coerce value :: Float))

clamp :: Scalar scalar => scalar -> scalar -> scalar -> scalar
clamp a b value
    | value < low = low
    | value > high = high
    | otherwise = value
  where
    low = min a b
    high = max a b

sqrt :: Sqrt scalar sqrtScalar => scalar -> sqrtScalar
sqrt value =
    coerce (Prelude.sqrt (coerce value :: Float))
