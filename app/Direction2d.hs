module Direction2d (
    Direction2d,
    x,
    positiveX,
    negativeX,
    y,
    positiveY,
    negativeY,
) where

import qualified Area
import Direction2d.Unsafe
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String

instance Negation (Direction2d coordinates) where
    negate (Direction2d x y) =
        Direction2d (negate x) (negate y)

instance DotProduct Direction2d Direction2d where
    type DotProductResult Direction2d Direction2d = Float
    (Direction2d x1 y1) . (Direction2d x2 y2) =
        x1 * x2 + y1 * y2

positiveX :: Direction2d coordinates
positiveX =
    Direction2d 1.0 0.0

negativeX :: Direction2d coordinates
negativeX =
    negate positiveX

positiveY :: Direction2d coordinates
positiveY =
    Direction2d 0.0 1.0

negativeY :: Direction2d coordinates
negativeY =
    negate positiveY

x :: Direction2d coordinates
x =
    positiveX

y :: Direction2d coordinates
y =
    positiveY

determinant :: Direction2d coordinates -> Direction2d coordinates -> Float
determinant d1 d2 =
    let (Direction2d x1 y1) = d1
        (Direction2d x2 y2) = d2
     in x1 * y2 - y1 * x2
