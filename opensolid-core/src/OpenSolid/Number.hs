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
  , pow
  , infinity
  , sign
  , isNaN
  , squared
  , cubed
  , abs
  , clampTo
  , interpolateFrom
  , midpoint
  , goldenRatio
  , sum
  , product
  , random
  , epsilon
  )
where

import {-# SOURCE #-} OpenSolid.Interval (Interval)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random.Internal qualified as Random
import OpenSolid.Text.Parse qualified as Text.Parse
import Prelude qualified

{-# INLINE fromDouble #-}
fromDouble :: Double -> Number
fromDouble = Quantity

{-# INLINE toDouble #-}
toDouble :: Number -> Double
toDouble (Quantity x) = x

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
infinity = Quantity.infinity

sign :: Number -> Sign
sign = Quantity.sign

isNaN :: Number -> Bool
isNaN = Quantity.isNaN

squared :: Number -> Number
squared = Quantity.squared

cubed :: Number -> Number
cubed value = value * value * value

abs :: Number -> Number
abs = Quantity.abs

clampTo :: Interval Unitless -> Number -> Number
clampTo = Quantity.clampTo

interpolateFrom :: Number -> Number -> Number -> Number
interpolateFrom = Quantity.interpolateFrom

midpoint :: Number -> Number -> Number
midpoint = Quantity.midpoint

pi :: Number
pi = Prelude.pi

halfPi :: Number
halfPi = 0.5 * pi

twoPi :: Number
twoPi = 2.0 * pi

goldenRatio :: Number
goldenRatio = 0.5 * (1.0 + sqrt 5.0)

sqrt :: Number -> Number
sqrt = Quantity.sqrt

hypot2 :: Number -> Number -> Number
hypot2 = Quantity.hypot2

hypot3 :: Number -> Number -> Number -> Number
hypot3 = Quantity.hypot3

sin :: Number -> Number
sin (Quantity x) = Quantity (Prelude.sin x)

cos :: Number -> Number
cos (Quantity x) = Quantity (Prelude.cos x)

tan :: Number -> Number
tan (Quantity x) = Quantity (Prelude.tan x)

asin :: Number -> Number
asin (Quantity x) = Quantity (Prelude.asin x)

acos :: Number -> Number
acos (Quantity x) = Quantity (Prelude.acos x)

atan :: Number -> Number
atan (Quantity x) = Quantity (Prelude.atan x)

atan2 :: Quantity units -> Quantity units -> Number
atan2 (Quantity y) (Quantity x) = Quantity (Prelude.atan2 y x)

log :: Number -> Number
log = Prelude.log

logBase :: Number -> Number -> Number
logBase = Prelude.logBase

pow :: Number -> Number -> Number
pow = (Prelude.**)

sum :: List Number -> Number
sum = Quantity.sum

product :: NonEmpty Number -> Number
product = NonEmpty.reduce (*)

random :: Number -> Number -> Random.Generator Number
random = Quantity.random

epsilon :: Number
epsilon = 2.2204460492503131e-16
