module OpenSolid.Quantity
  ( Quantity (Quantity, Quantity#)
  , zero
  , unit
  , infinity
  , coerce
  , sign
  , isNaN
  , isInfinite
  , interpolateFrom
  , midpoint
  , squared
  , squared_
  , sqrt
  , sqrt_
  , hypot2
  , hypot3
  , abs
  , minmax
  , clampTo
  , convert
  , unconvert
  , sum
  , sumOf
  , random
  , steps
  , leading
  , trailing
  , inBetween
  , midpoints
  )
where

import Data.Coerce qualified
import {-# SOURCE #-} OpenSolid.Interval (Interval)
import {-# SOURCE #-} OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Random.Internal qualified as Random
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import System.Random qualified
import Prelude qualified

{-# COMPLETE Quantity# #-}

{-# INLINE Quantity# #-}
pattern Quantity# :: Double# -> Quantity units
pattern Quantity# x# = Quantity (D# x#)

{-# INLINE zero #-}
zero :: Quantity units
zero = Quantity (D# 0.0##)

{-# INLINE unit #-}
unit :: Quantity units
unit = Quantity (D# 1.0##)

infinity :: Quantity units
infinity = Quantity (D# (1.0## /# 0.0##))

{-# INLINE coerce #-}
coerce :: Quantity units1 -> Quantity units2
coerce = Data.Coerce.coerce

sign :: Quantity units -> Sign
sign value = if value >= zero then Positive else Negative

{-# INLINE isNaN #-}
isNaN :: Quantity units -> Bool
isNaN (Quantity x) = Prelude.isNaN x

{-# INLINE isInfinite #-}
isInfinite :: Quantity units -> Bool
isInfinite (Quantity x) = Prelude.isInfinite x

{-# INLINE squared #-}
squared :: Units.Squared units1 units2 => Quantity units1 -> Quantity units2
squared x = x * x

{-# INLINE squared_ #-}
squared_ :: Quantity units -> Quantity (units ?*? units)
squared_ x = x ?*? x

sqrt :: Units.Squared units1 units2 => Quantity units2 -> Quantity units1
sqrt x | x <= zero = zero
sqrt (Quantity x) = Quantity (Prelude.sqrt x)

sqrt_ :: Quantity (units ?*? units) -> Quantity units
sqrt_ x | x <= zero = zero
sqrt_ (Quantity x) = Quantity (Prelude.sqrt x)

hypot2 :: Quantity units -> Quantity units -> Quantity units
hypot2 (Quantity# x#) (Quantity# y#) = Quantity# (hypot2# x# y#)

hypot3 :: Quantity units -> Quantity units -> Quantity units -> Quantity units
hypot3 (Quantity# x#) (Quantity# y#) (Quantity# z#) = Quantity# (hypot3# x# y# z#)

{-# INLINE abs #-}
abs :: Quantity units -> Quantity units
abs (Quantity x) = Quantity (Prelude.abs x)

clampTo :: Interval units -> Quantity units -> Quantity units
clampTo interval value = min (max (Interval.lower interval) value) (Interval.upper interval)

{-# INLINE minmax #-}
minmax :: (Quantity units, Quantity units) -> (Quantity units, Quantity units)
minmax (a, b) = if a <= b then (a, b) else (b, a)

-- | Interpolate from one value to another, based on a parameter that ranges from 0 to 1.
{-# INLINE interpolateFrom #-}
interpolateFrom :: Quantity units -> Quantity units -> Number -> Quantity units
interpolateFrom a b t = a + (b - a) * t

{-# INLINE midpoint #-}
midpoint :: Quantity units -> Quantity units -> Quantity units
midpoint a b = 0.5 * (a + b)

sum :: List (Quantity units) -> Quantity units
sum = List.foldl (+) zero

sumOf :: (a -> Quantity units) -> List a -> Quantity units
sumOf f list = sum (List.map f list)

convert :: Quantity (units2 ?/? units1) -> Quantity units1 -> Quantity units2
convert factor value = Units.simplify (value ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Quantity units2 -> Quantity units1
unconvert factor value = Units.simplify (value ?/? factor)

random :: Quantity units -> Quantity units -> Random.Generator (Quantity units)
random (Quantity low) (Quantity high) =
  Random.map Quantity (Random.Generator (System.Random.uniformR (low, high)))

{-| Interpolate between two values by subdividing into the given number of steps.

The result is an empty list if the given number of steps is zero (or negative).
Otherwise, the number of values in the resulting list will be equal to one plus the number of steps.
For example, for one step the returned values will just be the given start and end values;
for two steps the returned values will be the start value, the midpoint and then the end value.
-}
steps :: Quantity units -> Quantity units -> Int -> List (Quantity units)
steps start end n = if n > 0 then range start end n [0 .. n] else []

-- | Interpolate between two values like 'steps', but skip the first value.
leading :: Quantity units -> Quantity units -> Int -> List (Quantity units)
leading start end n = range start end n [0 .. n - 1]

-- | Interpolate between two values like 'steps', but skip the last value.
trailing :: Quantity units -> Quantity units -> Int -> List (Quantity units)
trailing start end n = range start end n [1 .. n]

-- | Interpolate between two values like 'steps', but skip the first and last values.
inBetween :: Quantity units -> Quantity units -> Int -> List (Quantity units)
inBetween start end n = range start end n [1 .. n - 1]

{-| Subdivide a given range into the given number of steps, and return the midpoint of each step.

This can be useful if you want to sample a curve or other function at the midpoint of several intervals.
-}
midpoints :: Quantity units -> Quantity units -> Int -> List (Quantity units)
midpoints start end n = range start end (2 * n) [1, 3 .. 2 * n - 1]

range :: Quantity units -> Quantity units -> Int -> List Int -> List (Quantity units)
range start end n indices = do
  let delta = end - start
  [start + (i / n) * delta | i <- indices]
