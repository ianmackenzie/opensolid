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

zero :: Quantity quantity => quantity
zero =
    coerce 0.0

positiveInfinity :: Quantity quantity => quantity
positiveInfinity =
    coerce (1.0 / 0.0)

negativeInfinity :: Quantity quantity => quantity
negativeInfinity =
    negate positiveInfinity

infinity :: Quantity quantity => quantity
infinity =
    positiveInfinity

isNaN :: Quantity quantity => quantity -> Bool
isNaN quantity =
    Prelude.isNaN (coerce quantity :: Float)

baseUnits :: Quantity quantity => Float -> quantity
baseUnits =
    coerce

inBaseUnits :: Quantity quantity => quantity -> Float
inBaseUnits =
    coerce

baseUnit :: Quantity quantity => quantity
baseUnit =
    baseUnits 1.0

interpolateFrom :: Quantity quantity => quantity -> quantity -> Float -> quantity
interpolateFrom a b t =
    if t <= 0.5
        then a + (b - a) * t
        else b + (a - b) * (1.0 - t)

midpoint :: Quantity quantity => quantity -> quantity -> quantity
midpoint a b =
    a + 0.5 * (b - a)

clamp :: Quantity quantity => quantity -> quantity -> quantity -> quantity
clamp low high value
    | value < low = low
    | value > high = high
    | otherwise = value
