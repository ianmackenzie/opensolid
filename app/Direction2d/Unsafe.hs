module Direction2d.Unsafe (Direction2d (..)) where

import OpenSolid
import Vector2d.Type

data Direction2d coordinates = Direction2d !Float !Float
    deriving (Eq, Show)

instance Negation (Direction2d coordinates) where
    negate (Direction2d x y) =
        Direction2d (negate x) (negate y)

instance DotProduct Direction2d Direction2d where
    type DotProductResult Direction2d Direction2d = Float
    (Direction2d x1 y1) . (Direction2d x2 y2) =
        x1 * x2 + y1 * y2

instance DotProduct (Vector2d units) Direction2d where
    type DotProductResult (Vector2d units) Direction2d = Quantity units
    (Vector2d vx vy) . (Direction2d dx dy) =
        vx * dx + vy * dy

instance DotProduct Direction2d (Vector2d units) where
    type DotProductResult Direction2d (Vector2d units) = Quantity units
    (Direction2d dx dy) . (Vector2d vx vy) =
        dx * vx + dy * vy

instance Multiplication (Quantity units) (Direction2d coordinates) where
    type Product (Quantity units) (Direction2d coordinates) = Vector2d units coordinates
    scale * (Direction2d x y) =
        Vector2d (scale * x) (scale * y)

instance Multiplication (Direction2d coordinates) (Quantity units) where
    type Product (Direction2d coordinates) (Quantity units) = Vector2d units coordinates
    (Direction2d x y) * scale =
        Vector2d (x * scale) (y * scale)
