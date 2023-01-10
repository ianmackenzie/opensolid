module Direction3d (
    Direction3d,
    unsafe,
) where

import OpenSolid

type role Direction3d phantom

data Direction3d coordinates

unsafe :: Float -> Float -> Float -> Direction3d coordinates
