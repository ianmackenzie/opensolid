module Qty (
    zero,
    infinity,
    sign,
    isNaN,
    interpolateFrom,
    midpoint,
    squared,
    sqrt,
    hypot2,
    hypot3,
    abs,
    clamp,
) where

import Data.Coerce (coerce)
import OpenSolid
import Prelude qualified

zero :: Qty units
zero = coerce 0.0

infinity :: Qty units
infinity = coerce (1.0 / 0.0)

sign :: Qty units -> Sign
sign value = if value >= zero then Positive else Negative

isNaN :: Qty units -> Bool
isNaN (Qty x) = Prelude.isNaN x

{-# INLINE squared #-}
squared :: Squared (Qty units1) (Qty units2) => Qty units1 -> Qty units2
squared x = x * x

sqrt :: Squared (Qty units1) (Qty units2) => Qty units2 -> Qty units1
sqrt x | x <= Qty.zero = Qty.zero
sqrt (Qty x) = Qty (Prelude.sqrt x)

hypot2 :: Qty units -> Qty units -> Qty units
hypot2 (Qty x) (Qty y) =
    Qty (Prelude.sqrt (x Prelude.* x Prelude.+ y Prelude.* y))

hypot3 :: Qty units -> Qty units -> Qty units -> Qty units
hypot3 (Qty x) (Qty y) (Qty z) =
    Qty (Prelude.sqrt (x Prelude.* x Prelude.+ y Prelude.* y Prelude.+ z Prelude.* z))

{-# INLINE abs #-}
abs :: Qty units -> Qty units
abs (Qty x) = Qty (Prelude.abs x)

clamp :: Qty units -> Qty units -> Qty units -> Qty units
clamp a b value
    | value < low = low
    | value > high = high
    | otherwise = value
  where
    low = min a b
    high = max a b

interpolateFrom :: Qty units -> Qty units -> Float -> Qty units
interpolateFrom a b t
    | t <= 0.5 = a + (b - a) * t
    | otherwise = b + (a - b) * (1.0 - t)

{-# INLINE midpoint #-}
midpoint :: Qty units -> Qty units -> Qty units
midpoint a b = 0.5 * (a + b)
