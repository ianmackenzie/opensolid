module OpenSolid.Float
  ( Float
  , fromRational
  , fromDouble
  , toDouble
  , int
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

import Data.Coerce qualified
import OpenSolid.Arithmetic
import OpenSolid.Bootstrap hiding (max, min, product)
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import OpenSolid.NonEmpty (NonEmpty)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Quantity (Quantity (Quantity))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random.Internal qualified as Random
import {-# SOURCE #-} OpenSolid.Result (Result)
import OpenSolid.Sign (Sign)
import OpenSolid.Text.Parse qualified as Text.Parse
import OpenSolid.Units (Unitless)
import Prelude qualified

type Float = Quantity Unitless

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational = Prelude.fromRational

{-# INLINE fromDouble #-}
fromDouble :: Prelude.Double -> Float
fromDouble = Data.Coerce.coerce

{-# INLINE toDouble #-}
toDouble :: Float -> Prelude.Double
toDouble = Data.Coerce.coerce

{-# INLINE int #-}
int :: Int -> Float
int = fromIntegral

parse :: Text -> Result Text Float
parse = Text.Parse.float

{-# INLINE floor #-}
floor :: Float -> Int
floor (Quantity x) = Prelude.floor x

{-# INLINE ceiling #-}
ceiling :: Float -> Int
ceiling (Quantity x) = Prelude.ceiling x

{-# INLINE round #-}
round :: Float -> Int
round (Quantity x) = Prelude.round x

infinity :: Float
infinity = Quantity.infinity

sign :: Float -> Sign
sign = Quantity.sign

isNaN :: Float -> Bool
isNaN = Quantity.isNaN

squared :: Float -> Float
squared = Quantity.squared

cubed :: Float -> Float
cubed value = value * value * value

abs :: Float -> Float
abs = Quantity.abs

clampTo :: Bounds Unitless -> Float -> Float
clampTo = Quantity.clampTo

interpolateFrom :: Float -> Float -> Float -> Float
interpolateFrom = Quantity.interpolateFrom

midpoint :: Float -> Float -> Float
midpoint = Quantity.midpoint

min :: Float -> Float -> Float
min = Quantity.min

max :: Float -> Float -> Float
max = Quantity.max

pi :: Float
pi = Prelude.pi

halfPi :: Float
halfPi = 0.5 * pi

twoPi :: Float
twoPi = 2.0 * pi

goldenRatio :: Float
goldenRatio = 0.5 * (1.0 + sqrt 5.0)

sqrt :: Float -> Float
sqrt = Quantity.sqrt

hypot2 :: Float -> Float -> Float
hypot2 = Quantity.hypot2

hypot3 :: Float -> Float -> Float -> Float
hypot3 = Quantity.hypot3

sin :: Float -> Float
sin (Quantity x) = Quantity (Prelude.sin x)

cos :: Float -> Float
cos (Quantity x) = Quantity (Prelude.cos x)

tan :: Float -> Float
tan (Quantity x) = Quantity (Prelude.tan x)

asin :: Float -> Float
asin (Quantity x) = Quantity (Prelude.asin x)

acos :: Float -> Float
acos (Quantity x) = Quantity (Prelude.acos x)

atan :: Float -> Float
atan (Quantity x) = Quantity (Prelude.atan x)

atan2 :: Quantity units -> Quantity units -> Float
atan2 (Quantity y) (Quantity x) = Quantity (Prelude.atan2 y x)

log :: Float -> Float
log (Quantity x) = Quantity (Prelude.log x)

logBase :: Float -> Float -> Float
logBase (Quantity base) (Quantity x) = Quantity (Prelude.logBase base x)

sum :: List Float -> Float
sum = Quantity.sum

product :: NonEmpty Float -> Float
product = NonEmpty.reduce (*)

random :: Float -> Float -> Random.Generator Float
random = Quantity.random

epsilon :: Float
epsilon = 2.2204460492503131e-16
