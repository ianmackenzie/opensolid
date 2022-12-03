module Direction2d (
    Direction2d,
    x,
    positiveX,
    negativeX,
    y,
    positiveY,
    negativeY,
    determinant,
) where

import Direction2d.Unsafe
import OpenSolid

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
