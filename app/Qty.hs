module Qty (
    zero,
    infinity,
    isNaN,
    interpolateFrom,
    midpoint,
    sqrt,
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
    atan2,
    abs,
    clamp,
) where

import OpenSolid

interpolateFrom :: Qty units -> Qty units -> Float -> Qty units
interpolateFrom a b t
    | t <= 0.5 = a + (b - a) * t
    | otherwise = b + (a - b) * (1.0 - t)

midpoint :: Qty units -> Qty units -> Qty units
midpoint a b = 0.5 * (a + b)
