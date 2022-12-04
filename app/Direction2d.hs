module Direction2d (
    Direction2d,
    unsafe,
    x,
    positiveX,
    negativeX,
    y,
    positiveY,
    negativeY,
    determinant,
) where

import OpenSolid
import Vector2d (Vector2d (..))

data Direction2d coordinates = Direction2d !Float !Float
    deriving (Eq, Show)

instance Negation (Direction2d coordinates) where
    negate (Direction2d dx dy) = Direction2d (negate dx) (negate dy)

instance DotProduct Direction2d Direction2d Float where
    (Direction2d x1 y1) <> (Direction2d x2 y2) = x1 * x2 + y1 * y2

instance DotProduct (Vector2d units) Direction2d (Qty units) where
    (Vector2d vx vy) <> (Direction2d dx dy) = vx * dx + vy * dy

instance DotProduct Direction2d (Vector2d units) (Qty units) where
    (Direction2d dx dy) <> (Vector2d vx vy) = dx * vx + dy * vy

instance Multiplication (Qty units) (Direction2d coordinates) (Vector2d units coordinates) where
    scale * (Direction2d dx dy) = Vector2d (scale * dx) (scale * dy)

instance Multiplication (Direction2d coordinates) (Qty units) (Vector2d units coordinates) where
    (Direction2d dx dy) * scale = Vector2d (dx * scale) (dy * scale)

unsafe :: Float -> Float -> Direction2d coordinates
unsafe = Direction2d

positiveX :: Direction2d coordinates
positiveX = Direction2d 1.0 0.0

negativeX :: Direction2d coordinates
negativeX = negate positiveX

positiveY :: Direction2d coordinates
positiveY = Direction2d 0.0 1.0

negativeY :: Direction2d coordinates
negativeY = negate positiveY

x :: Direction2d coordinates
x = positiveX

y :: Direction2d coordinates
y = positiveY

determinant :: Direction2d coordinates -> Direction2d coordinates -> Float
determinant (Direction2d x1 y1) (Direction2d x2 y2) = x1 * y2 - y1 * x2
