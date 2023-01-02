module Direction2d (
    Direction2d,
    unsafe,
) where

import OpenSolid

data Direction2d coordinates = Direction2d Float Float

unsafe :: Float -> Float -> Direction2d coordinates
