module OpenSolid.Quantity
  ( Quantity (Quantity, Q#)
  , zero
  , unit
  , infinity
  , coerce
  , erase
  , unerase
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
  , convert
  , unconvert
  , sum
  , sumOf
  , steps
  , leading
  , trailing
  , inBetween
  , midpoints
  )
where

import Data.Coerce qualified
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

{-# INLINE zero #-}
zero :: Quantity units
zero = Quantity 0.0

{-# INLINE unit #-}
unit :: Quantity units
unit = Quantity 1.0

infinity :: Quantity units
infinity = Quantity Number.infinity

{-# INLINE coerce #-}
coerce :: Quantity units1 -> Quantity units2
coerce = Data.Coerce.coerce

{-# INLINE erase #-}
erase :: Quantity units -> Number
erase = coerce

{-# INLINE unerase #-}
unerase :: Number -> Quantity units
unerase = coerce

{-# INLINE sign #-}
sign :: Quantity units -> Sign
sign = Data.Coerce.coerce Number.sign

{-# INLINE isNaN #-}
isNaN :: Quantity units -> Bool
isNaN = Data.Coerce.coerce Number.isNaN

{-# INLINE isInfinite #-}
isInfinite :: Quantity units -> Bool
isInfinite = Data.Coerce.coerce Number.isInfinite

{-# INLINE squared #-}
squared :: Units.Squared units1 units2 => Quantity units1 -> Quantity units2
squared = coerce . squared_

{-# INLINE squared_ #-}
squared_ :: Quantity units -> Quantity (units ?*? units)
squared_ = Data.Coerce.coerce Number.squared

{-# INLINE sqrt #-}
sqrt :: Units.Squared units1 units2 => Quantity units2 -> Quantity units1
sqrt = sqrt_ . coerce

{-# INLINE sqrt_ #-}
sqrt_ :: Quantity (units ?*? units) -> Quantity units
sqrt_ = Data.Coerce.coerce Number.sqrt

{-# INLINE hypot2 #-}
hypot2 :: Quantity units -> Quantity units -> Quantity units
hypot2 = Data.Coerce.coerce Number.hypot2

{-# INLINE hypot3 #-}
hypot3 :: Quantity units -> Quantity units -> Quantity units -> Quantity units
hypot3 = Data.Coerce.coerce Number.hypot3

{-# INLINE abs #-}
abs :: Quantity units -> Quantity units
abs = Data.Coerce.coerce Number.abs

{-# INLINE minmax #-}
minmax :: (Quantity units, Quantity units) -> (Quantity units, Quantity units)
minmax (a, b) = if a <= b then (a, b) else (b, a)

-- | Interpolate from one value to another, based on a parameter that ranges from 0 to 1.
{-# INLINE interpolateFrom #-}
interpolateFrom :: Quantity units -> Quantity units -> Number -> Quantity units
interpolateFrom = Data.Coerce.coerce Number.interpolateFrom

{-# INLINE midpoint #-}
midpoint :: Quantity units -> Quantity units -> Quantity units
midpoint = Data.Coerce.coerce Number.midpoint

sum :: List (Quantity units) -> Quantity units
sum = Data.Coerce.coerce Number.sum

sumOf :: (a -> Quantity units) -> List a -> Quantity units
sumOf = Data.Coerce.coerce Number.sumOf

convert :: Quantity (units2 ?/? units1) -> Quantity units1 -> Quantity units2
convert factor value = Units.simplify (value ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Quantity units2 -> Quantity units1
unconvert factor value = Units.simplify (value ?/? factor)

{-| Interpolate between two values by subdividing into the given number of steps.

The result is an empty list if the given number of steps is zero (or negative).
Otherwise, the number of values in the resulting list will be equal to one plus the number of steps.
For example, for one step the returned values will just be the given start and end values;
for two steps the returned values will be the start value, the midpoint and then the end value.
-}
steps :: Quantity units -> Quantity units -> Int -> List (Quantity units)
steps = Data.Coerce.coerce Number.steps

-- | Interpolate between two values like 'steps', but skip the first value.
leading :: Quantity units -> Quantity units -> Int -> List (Quantity units)
leading = Data.Coerce.coerce Number.leading

-- | Interpolate between two values like 'steps', but skip the last value.
trailing :: Quantity units -> Quantity units -> Int -> List (Quantity units)
trailing = Data.Coerce.coerce Number.trailing

-- | Interpolate between two values like 'steps', but skip the first and last values.
inBetween :: Quantity units -> Quantity units -> Int -> List (Quantity units)
inBetween = Data.Coerce.coerce Number.inBetween

{-| Subdivide a given range into the given number of steps, and return the midpoint of each step.

This can be useful if you want to sample a curve or other function at the midpoint of several intervals.
-}
midpoints :: Quantity units -> Quantity units -> Int -> List (Quantity units)
midpoints = Data.Coerce.coerce Number.midpoints
