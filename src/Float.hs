module Float
  ( Float
  , fromRational
  , fromInt
  , ceiling
  , floor
  , pi
  , tau
  , pow
  )
where

import Arithmetic
import Basics
import Qty (Qty (..))
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

pi :: Float
pi = Prelude.pi

tau :: Float
tau = 2.0 * pi

pow :: Float -> Float -> Float
pow = (Prelude.**)
