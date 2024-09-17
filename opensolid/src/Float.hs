module Float
  ( Float
  , pattern Int
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
  , infinity
  , sign
  , isNaN
  , squared
  , abs
  , clamp
  , interpolateFrom
  , midpoint
  , goldenRatio
  , sum
  , product
  , random
  , epsilon
  )
where

import Arithmetic
import Basics
import Data.Coerce qualified
import NonEmpty (NonEmpty)
import NonEmpty qualified
import Qty (Qty (Qty))
import Qty qualified
import Random.Internal qualified as Random
import {-# SOURCE #-} Result (Result)
import Sign (Sign)
import Text.Parse qualified
import Units (Unitless)
import Prelude qualified

type Float = Qty Unitless

pattern Int :: Int -> Float
pattern Int n <- (toInt -> Just n)

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational = Prelude.fromRational

{-# INLINE fromDouble #-}
fromDouble :: Prelude.Double -> Float
fromDouble = Data.Coerce.coerce

{-# INLINE toDouble #-}
toDouble :: Float -> Prelude.Double
toDouble = Data.Coerce.coerce

int :: Int -> Float
int = fromIntegral

toInt :: Float -> Maybe Int
toInt value = do
  let candidate = round value
  if int candidate == value then Just candidate else Nothing

parse :: Text -> Result Text Float
parse = Text.Parse.float

{-# INLINE floor #-}
floor :: Float -> Int
floor (Qty x) = Prelude.floor x

{-# INLINE ceiling #-}
ceiling :: Float -> Int
ceiling (Qty x) = Prelude.ceiling x

{-# INLINE round #-}
round :: Float -> Int
round (Qty x) = Prelude.round x

infinity :: Float
infinity = Qty.infinity

sign :: Float -> Sign
sign = Qty.sign

isNaN :: Float -> Bool
isNaN = Qty.isNaN

squared :: Float -> Float
squared = Qty.squared

abs :: Float -> Float
abs = Qty.abs

clamp :: Float -> Float -> Float -> Float
clamp = Qty.clamp

interpolateFrom :: Float -> Float -> Float -> Float
interpolateFrom = Qty.interpolateFrom

midpoint :: Float -> Float -> Float
midpoint = Qty.midpoint

min :: Float -> Float -> Float
min = Qty.min

max :: Float -> Float -> Float
max = Qty.max

pi :: Float
pi = Prelude.pi

twoPi :: Float
twoPi = 2 * pi

goldenRatio :: Float
goldenRatio = 0.5 * (1.0 + sqrt 5.0)

sqrt :: Float -> Float
sqrt = Qty.sqrt

hypot2 :: Float -> Float -> Float
hypot2 = Qty.hypot2

hypot3 :: Float -> Float -> Float -> Float
hypot3 = Qty.hypot3

sin :: Float -> Float
sin (Qty x) = Qty (Prelude.sin x)

cos :: Float -> Float
cos (Qty x) = Qty (Prelude.cos x)

tan :: Float -> Float
tan (Qty x) = Qty (Prelude.tan x)

asin :: Float -> Float
asin (Qty x) = Qty (Prelude.asin x)

acos :: Float -> Float
acos (Qty x) = Qty (Prelude.acos x)

atan :: Float -> Float
atan (Qty x) = Qty (Prelude.atan x)

atan2 :: Qty units -> Qty units -> Float
atan2 (Qty y) (Qty x) = Qty (Prelude.atan2 y x)

sum :: List Float -> Float
sum = Qty.sum

product :: NonEmpty Float -> Float
product = NonEmpty.reduce (*)

random :: Float -> Float -> Random.Generator Float
random = Qty.random

epsilon :: Float
epsilon = 2.2204460492503131e-16
