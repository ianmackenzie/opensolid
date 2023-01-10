module Float (
    ceiling,
    floor,
    pi,
    pow,
) where

import OpenSolid
import Prelude qualified

{-# INLINE floor #-}
floor :: Float -> Int
floor (Qty x) = Nbr (Prelude.floor x)

{-# INLINE ceiling #-}
ceiling :: Float -> Int
ceiling (Qty x) = Nbr (Prelude.ceiling x)

pi :: Float
pi = Prelude.pi

pow :: Float -> Float -> Float
pow = (Prelude.**)
