module Quantity (
    Quantity,
    zero,
    infinity,
    positiveInfinity,
    negativeInfinity,
    isNaN,
    baseUnit,
    baseUnits,
    inBaseUnits,
    interpolateFrom,
    midpoint,
    clamp,
) where

import Data.Coerce (coerce)
import OpenSolid
import qualified Prelude

zero :: Quantity units
zero =
    coerce 0.0

positiveInfinity :: Quantity units
positiveInfinity =
    coerce (1.0 / 0.0)

negativeInfinity :: Quantity units
negativeInfinity =
    negate positiveInfinity

infinity :: Quantity units
infinity =
    positiveInfinity

isNaN :: Quantity units -> Bool
isNaN (Quantity value) =
    Prelude.isNaN value

baseUnits :: Float -> Quantity units
baseUnits =
    coerce

inBaseUnits :: Quantity units -> Float
inBaseUnits =
    coerce

baseUnit :: Quantity units
baseUnit =
    baseUnits 1.0

interpolateFrom :: Quantity units -> Quantity units -> Float -> Quantity units
interpolateFrom a b t =
    if t <= 0.5
        then a + (b - a) * t
        else b + (a - b) * (1.0 - t)

midpoint :: Quantity units -> Quantity units -> Quantity units
midpoint a b =
    a + 0.5 * (b - a)

clamp :: Quantity units -> Quantity units -> Quantity units -> Quantity units
clamp low high value
    | value < low = low
    | value > high = high
    | otherwise = value
