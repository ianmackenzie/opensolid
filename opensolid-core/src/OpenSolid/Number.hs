module OpenSolid.Number
  ( Number
  , fromDouble
  , toDouble
  , fromInt
  , parse
  , ceiling
  , round
  , min
  , max
  , floor
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

import Data.Text (Text)
import OpenSolid.Arithmetic
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import OpenSolid.List (List)
import OpenSolid.NonEmpty (NonEmpty)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Quantity (Quantity (Quantity))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random.Internal qualified as Random
import {-# SOURCE #-} OpenSolid.Result (Result)
import OpenSolid.Sign (Sign)
import OpenSolid.Text.Parse qualified as Text.Parse
import OpenSolid.Units (Unitless)
import Prelude (Bool, Double, Int)
import Prelude qualified

type Number = Quantity Unitless

{-# INLINE fromDouble #-}
fromDouble :: Double -> Number
fromDouble = Quantity

{-# INLINE toDouble #-}
toDouble :: Number -> Double
toDouble (Quantity x) = x

{-# INLINE fromInt #-}
fromInt :: Int -> Number
fromInt = Prelude.fromIntegral

parse :: Text -> Result Text Number
parse = Text.Parse.number

{-# INLINE floor #-}
floor :: Number -> Int
floor (Quantity x) = Prelude.floor x

{-# INLINE ceiling #-}
ceiling :: Number -> Int
ceiling (Quantity x) = Prelude.ceiling x

{-# INLINE round #-}
round :: Number -> Int
round (Quantity x) = Prelude.round x

infinity :: Number
infinity = Quantity.infinity

sign :: Number -> Sign
sign = Quantity.sign

isNaN :: Number -> Bool
isNaN = Quantity.isNaN

squared :: Number -> Number
squared = Quantity.squared

cubed :: Number -> Number
cubed value = value .*. value .*. value

abs :: Number -> Number
abs = Quantity.abs

clampTo :: Bounds Unitless -> Number -> Number
clampTo = Quantity.clampTo

interpolateFrom :: Number -> Number -> Number -> Number
interpolateFrom = Quantity.interpolateFrom

midpoint :: Number -> Number -> Number
midpoint = Quantity.midpoint

min :: Number -> Number -> Number
min = Quantity.min

max :: Number -> Number -> Number
max = Quantity.max

pi :: Number
pi = Prelude.pi

halfPi :: Number
halfPi = 0.5 *. pi

twoPi :: Number
twoPi = 2 *. pi

goldenRatio :: Number
goldenRatio = 0.5 *. (1 +. sqrt 5)

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
product = NonEmpty.reduce (.*.)

random :: Number -> Number -> Random.Generator Number
random = Quantity.random

epsilon :: Number
epsilon = 2.2204460492503131e-16
