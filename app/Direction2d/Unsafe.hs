module Direction2d.Unsafe (Direction2d (..)) where

import OpenSolid
import Vector2d.Type

data Direction2d coordinates = Direction2d !Float !Float
    deriving (Eq, Show)

instance Negation (Direction2d coordinates) where
    negate (Direction2d x y) =
        Direction2d (negate x) (negate y)

instance DotProduct Direction2d Direction2d Float where
    (Direction2d x1 y1) . (Direction2d x2 y2) =
        x1 * x2 + y1 * y2

instance Scalar scalar => DotProduct (Vector2d scalar) Direction2d scalar where
    (Vector2d vx vy) . (Direction2d dx dy) =
        vx * dx + vy * dy

instance Scalar scalar => DotProduct Direction2d (Vector2d scalar) scalar where
    (Direction2d dx dy) . (Vector2d vx vy) =
        dx * vx + dy * vy

instance Scalar scalar => Multiplication scalar (Direction2d coordinates) (Vector2d scalar coordinates) where
    scale * (Direction2d x y) =
        Vector2d (scale * x) (scale * y)

instance Scalar scalar => Multiplication (Direction2d coordinates) scalar (Vector2d scalar coordinates) where
    (Direction2d x y) * scale =
        Vector2d (x * scale) (y * scale)
