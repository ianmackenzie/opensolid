module Float (
    ceiling,
    floor,
    pi,
) where

import OpenSolid
import Prelude qualified

floor :: Float -> Int
floor (Qty x) = Nbr (Prelude.floor x)

ceiling :: Float -> Int
ceiling (Qty x) = Nbr (Prelude.ceiling x)

pi :: Float
pi = Prelude.pi
