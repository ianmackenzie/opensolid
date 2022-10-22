module Direction2d.Unsafe (Direction2d (..)) where

import OpenSolid

data Direction2d coordinates = Direction2d !Float !Float
    deriving (Eq, Show)

instance Negation (Direction2d coordinates) where
    negate (Direction2d x y) =
        Direction2d (negate x) (negate y)

instance DotProduct Direction2d Direction2d where
    type DotProductResult Direction2d Direction2d = Float
    (Direction2d x1 y1) . (Direction2d x2 y2) =
        x1 * x2 + y1 * y2
