module Direction3d (
    Direction3d,
    unsafe,
) where

import OpenSolid

data Direction3d coordinates = Direction3d Float Float Float

unsafe :: Float -> Float -> Float -> Direction3d coordinates
