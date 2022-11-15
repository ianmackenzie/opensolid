module Qty (
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

zero :: Qty units
zero =
    coerce 0.0

positiveInfinity :: Qty units
positiveInfinity =
    coerce (1.0 / 0.0)

negativeInfinity :: Qty units
negativeInfinity =
    negate positiveInfinity

infinity :: Qty units
infinity =
    positiveInfinity

isNaN :: Qty units -> Bool
isNaN value =
    Prelude.isNaN (unQty value)

interpolateFrom :: Qty units -> Qty units -> Float -> Qty units
interpolateFrom a b t =
    if t <= 0.5
        then a + (b - a) * t
        else b + (a - b) * (1.0 - t)

midpoint :: Qty units -> Qty units -> Qty units
midpoint a b =
    0.5 * (a + b)

abs :: Qty units -> Qty units
abs value =
    Qty (Prelude.abs (unQty value))

clamp :: Qty units -> Qty units -> Qty units -> Qty units
clamp a b value
    | value < low = low
    | value > high = high
    | otherwise = value
  where
    low = min a b
    high = max a b

sqrt :: Sqrt (Qty units) (Qty b) => Qty units -> Qty b
sqrt value =
    Qty (Prelude.sqrt (unQty value))
