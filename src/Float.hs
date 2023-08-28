module Float
  ( Float
  , fromRational
  , fromInt
  , ceiling
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
  )
where

import Arithmetic
import Basics
import Qty (Qty (..))
import Qty qualified
import Units (Unitless)
import Prelude qualified

type Float = Qty Unitless

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational = Prelude.fromRational

fromInt :: Int -> Float
fromInt = Prelude.fromIntegral

{-# INLINE floor #-}
floor :: Float -> Int
floor (Qty x) = Prelude.floor x

{-# INLINE ceiling #-}
ceiling :: Float -> Int
ceiling (Qty x) = Prelude.ceiling x

min :: Float -> Float -> Float
min = Qty.min

max :: Float -> Float -> Float
max = Qty.max

pi :: Float
pi = Prelude.pi

twoPi :: Float
twoPi = 2.0 * pi

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
