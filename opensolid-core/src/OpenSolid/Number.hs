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
  , squared
  , cubed
  , abs
  , clampTo
  , interpolateFrom
  , midpoint
  , goldenRatio
  , sum
  , product
  , epsilon
  )
where

import {-# SOURCE #-} OpenSolid.Interval (Interval)
import {-# SOURCE #-} OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Text.Parse qualified as Text.Parse
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
clampTo interval value = min (max (Interval.lower interval) value) (Interval.upper interval)

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
sin = Prelude.sin

cos :: Number -> Number
cos = Prelude.cos

tan :: Number -> Number
tan = Prelude.tan

asin :: Number -> Number
asin = Prelude.asin

acos :: Number -> Number
acos = Prelude.acos

atan :: Number -> Number
atan = Prelude.atan

atan2 :: Quantity units -> Quantity units -> Number
atan2 (Quantity y) (Quantity x) = Prelude.atan2 y x

log :: Number -> Number
log = Prelude.log

logBase :: Number -> Number -> Number
logBase = Prelude.logBase

sum :: List Number -> Number
sum = Quantity.sum

product :: NonEmpty Number -> Number
product = NonEmpty.reduce (*)

epsilon :: Number
epsilon = 2.2204460492503131e-16
