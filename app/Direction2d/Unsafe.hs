module Direction2d.Unsafe (Direction2d (..)) where

import OpenSolid

data Direction2d coordinates = Direction2d Float Float
    deriving (Eq)
