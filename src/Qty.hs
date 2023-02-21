module Qty
  ( zero
  , infinity
  , sign
  , isNaN
  , interpolateFrom
  , midpoint
  , squared
  , sqrt
  , hypot2
  , hypot3
  , abs
  , clamp
  )
where

import Data.Coerce (coerce)
import OpenSolid
import Units qualified
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
squared :: Units.Squared units1 units2 => Qty units1 -> Qty units2
squared x = x * x

sqrt :: Units.Squared units1 units2 => Qty units2 -> Qty units1
sqrt x | x <= Qty.zero = Qty.zero
sqrt (Qty x) = Qty (Prelude.sqrt x)

hypot2 :: Qty units -> Qty units -> Qty units
hypot2 x y =
  let x2 = Qty.squared (Units.generalize x)
      y2 = Qty.squared (Units.generalize y)
   in Units.specialize (sqrt (x2 + y2))

hypot3 :: Qty units -> Qty units -> Qty units -> Qty units
hypot3 x y z =
  let x2 = Qty.squared (Units.generalize x)
      y2 = Qty.squared (Units.generalize y)
      z2 = Qty.squared (Units.generalize z)
   in Units.specialize (sqrt (x2 + y2 + z2))

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
interpolateFrom a b t =
  if t <= 0.5
    then a + (b - a) * t
    else b + (a - b) * (1.0 - t)

{-# INLINE midpoint #-}
midpoint :: Qty units -> Qty units -> Qty units
midpoint a b = 0.5 * (a + b)
