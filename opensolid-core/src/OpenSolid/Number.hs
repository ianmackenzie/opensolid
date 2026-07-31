module OpenSolid.Number
  ( Number
  , fromDouble
  , toDouble
  , fromInt
  , floor
  , ceiling
  , round
  , parse
  , pi
  , halfPi
  , twoPi
  , sqrt
  , hypot2
  , hypot3
  , sin
  , cos
  , tan
  , asin
  , acos
  , atan
  , atan2
  , log
  , logBase
  , infinity
  , sign
  , isNaN
  , isInfinite
  , squared
  , cubed
  , abs
  , clampTo
  , interpolateFrom
  , midpoint
  , goldenRatio
  , sum
  , sumOf
  , product
  , productOf
  , epsilon
  , steps
  , leading
  , trailing
  , inBetween
  , midpoints
  )
where

import {-# SOURCE #-} OpenSolid.Interval (Interval)
import {-# SOURCE #-} OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Text.Parse qualified as Text.Parse
import OpenSolid.Unboxed.Math
import Prelude qualified

{-# INLINE fromDouble #-}
fromDouble :: Double -> Number
fromDouble = Number

{-# INLINE toDouble #-}
toDouble :: Number -> Double
toDouble (Number double) = double

{-# INLINE fromInt #-}
fromInt :: Int -> Number
fromInt = Prelude.fromIntegral

{-# INLINE floor #-}
floor :: Number -> Int
floor = Prelude.floor

{-# INLINE ceiling #-}
ceiling :: Number -> Int
ceiling = Prelude.ceiling

{-# INLINE round #-}
round :: Number -> Int
round = Prelude.round

parse :: Text -> Result Text Number
parse = Text.Parse.number

infinity :: Number
infinity = 1.0 / 0.0

{-# INLINE sign #-}
sign :: Number -> Sign
sign value = if value < 0.0 then Negative else Positive

{-# INLINE isNaN #-}
isNaN :: Number -> Bool
isNaN = Prelude.isNaN

{-# INLINE isInfinite #-}
isInfinite :: Number -> Bool
isInfinite = Prelude.isInfinite

{-# INLINE squared #-}
squared :: Number -> Number
squared value = value * value

{-# INLINE cubed #-}
cubed :: Number -> Number
cubed value = value * value * value

{-# INLINE abs #-}
abs :: Number -> Number
abs = Prelude.abs

clampTo :: Interval Unitless -> Number -> Number
clampTo interval value = min (max (Interval.lower interval) value) (Interval.upper interval)

{-# INLINE interpolateFrom #-}
interpolateFrom :: Number -> Number -> Number -> Number
interpolateFrom a b t = a + (b - a) * t

{-# INLINE midpoint #-}
midpoint :: Number -> Number -> Number
midpoint a b = 0.5 * (a + b)

pi :: Number
pi = Prelude.pi

halfPi :: Number
halfPi = 0.5 * pi

twoPi :: Number
twoPi = 2.0 * pi

goldenRatio :: Number
goldenRatio = 0.5 * (1.0 + sqrt 5.0)

{-# INLINE sqrt #-}
sqrt :: Number -> Number
sqrt x = if x <= 0.0 then 0.0 else Prelude.sqrt x

{-# INLINE hypot2 #-}
hypot2 :: Number -> Number -> Number
hypot2 (N# x#) (N# y#) = N# (hypot2# x# y#)

{-# INLINE hypot3 #-}
hypot3 :: Number -> Number -> Number -> Number
hypot3 (N# x#) (N# y#) (N# z#) = N# (hypot3# x# y# z#)

{-# INLINE sin #-}
sin :: Number -> Number
sin = Prelude.sin

{-# INLINE cos #-}
cos :: Number -> Number
cos = Prelude.cos

{-# INLINE tan #-}
tan :: Number -> Number
tan = Prelude.tan

{-# INLINE asin #-}
asin :: Number -> Number
asin = Prelude.asin

{-# INLINE acos #-}
acos :: Number -> Number
acos = Prelude.acos

{-# INLINE atan #-}
atan :: Number -> Number
atan = Prelude.atan

{-# INLINE atan2 #-}
atan2 :: Quantity units -> Quantity units -> Number
atan2 (Quantity y) (Quantity x) = Prelude.atan2 y x

{-# INLINE log #-}
log :: Number -> Number
log = Prelude.log

{-# INLINE logBase #-}
logBase :: Number -> Number -> Number
logBase = Prelude.logBase

sum :: List Number -> Number
sum = Prelude.sum

sumOf :: (a -> Number) -> List a -> Number
sumOf f = sum . List.map f

product :: NonEmpty Number -> Number
product = Prelude.product

productOf :: (a -> Number) -> NonEmpty a -> Number
productOf f = product . NonEmpty.map f

epsilon :: Number
epsilon = 2.2204460492503131e-16

{-| Interpolate between two values by subdividing into the given number of steps.

The result is an empty list if the given number of steps is zero (or negative).
Otherwise, the number of values in the resulting list will be equal to one plus the number of steps.
For example, for one step the returned values will just be the given start and end values;
for two steps the returned values will be the start value, the midpoint and then the end value.
-}
steps :: Number -> Number -> Int -> List Number
steps start end n = if n > 0 then range start end n [0 .. n] else []

-- | Interpolate between two values like 'steps', but skip the first value.
leading :: Number -> Number -> Int -> List Number
leading start end n = range start end n [0 .. n - 1]

-- | Interpolate between two values like 'steps', but skip the last value.
trailing :: Number -> Number -> Int -> List Number
trailing start end n = range start end n [1 .. n]

-- | Interpolate between two values like 'steps', but skip the first and last values.
inBetween :: Number -> Number -> Int -> List Number
inBetween start end n = range start end n [1 .. n - 1]

{-| Subdivide a given range into the given number of steps, and return the midpoint of each step.

This can be useful if you want to sample a curve or other function at the midpoint of several intervals.
-}
midpoints :: Number -> Number -> Int -> List Number
midpoints start end n = range start end (2 * n) [1, 3 .. 2 * n - 1]

range :: Number -> Number -> Int -> List Int -> List Number
range start end n indices = do
  let delta = end - start
  [start + (i / n) * delta | i <- indices]
