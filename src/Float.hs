module Float
  ( ceiling
  , floor
  , pi
  , tau
  , pow
  )
where

import OpenSolid
import Prelude qualified

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
