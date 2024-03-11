module Float
  ( Float
  , fromRational
  , fromInt
  , parse
  , ceiling
  , round
  , min
  , max
  , floor
  , pi
  , twoPi
  , sqrt
  , pow
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
  )
where

import Arithmetic
import Basics
import Concatenation
import {-# SOURCE #-} Maybe qualified
import NonEmpty (NonEmpty)
import NonEmpty qualified
import Qty (Qty (Qty))
import Qty qualified
import {-# SOURCE #-} Result (Result)
import Sign (Sign)
import Text.Read qualified
import {-# SOURCE #-} Units (Unitless)
import Prelude qualified

type Float = Qty Unitless

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational = Prelude.fromRational

fromInt :: Int -> Float
fromInt = fromIntegral

parse :: String -> Result String Float
parse input =
  Text.Read.readMaybe input
    |> Maybe.orError ("Couldn't parse input as a float: " ++ input)

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
twoPi = 2.0 * pi

goldenRatio :: Float
goldenRatio = 0.5 * (1.0 + sqrt 5.0)

sqrt :: Float -> Float
sqrt = Qty.sqrt

pow :: Float -> Float -> Float
pow = (Prelude.**)

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
