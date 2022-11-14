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

zero :: Qty a
zero =
    coerce 0.0

positiveInfinity :: Qty a
positiveInfinity =
    coerce (1.0 / 0.0)

negativeInfinity :: Qty a
negativeInfinity =
    negate positiveInfinity

infinity :: Qty a
infinity =
    positiveInfinity

isNaN :: Qty a -> Bool
isNaN value =
    Prelude.isNaN (unQty value)

interpolateFrom :: Qty a -> Qty a -> Float -> Qty a
interpolateFrom a b t =
    if t <= 0.5
        then a + (b - a) * t
        else b + (a - b) * (1.0 - t)

midpoint :: Qty a -> Qty a -> Qty a
midpoint a b =
    0.5 * (a + b)

abs :: Qty a -> Qty a
abs value =
    coerce (Prelude.abs (unQty value))

clamp :: Qty a -> Qty a -> Qty a -> Qty a
clamp a b value
    | value < low = low
    | value > high = high
    | otherwise = value
  where
    low = min a b
    high = max a b

sqrt :: Sqrt (Qty a) (Qty b) => Qty a -> Qty b
sqrt value =
    coerce (Prelude.sqrt (unQty value))
