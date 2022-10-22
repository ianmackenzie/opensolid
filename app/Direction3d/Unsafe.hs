module Direction3d.Unsafe (Direction3d (..)) where

import OpenSolid

data Direction3d coordinates = Direction3d !Float !Float !Float
    deriving (Eq, Show)

instance Negation (Direction3d coordinates) where
    negate (Direction3d x y z) =
        Direction3d (negate x) (negate y) (negate z)

instance DotProduct Direction3d Direction3d where
    type DotProductResult Direction3d Direction3d = Float
    (Direction3d x1 y1 z1) . (Direction3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2
