module Direction3d.Unsafe (Direction3d (..)) where

import OpenSolid

data Direction3d coordinates = Direction3d Float Float Float
    deriving (Eq, Show)
