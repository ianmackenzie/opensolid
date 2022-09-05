module Direction2d (
    Direction2d,
    x,
    y,
) where

import qualified Area
import Direction2d.Unsafe
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String

instance Show (Direction2d coordinates) where
    show (Direction2d x y) =
        let xString = String.fromFloat x
            yString = String.fromFloat y
         in String.toList ("Direction2d (" ++ xString ++ ", " ++ yString ++ ")")

instance Negation (Direction2d coordinates) where
    negate (Direction2d x y) =
        Direction2d (- x) (- y)

instance DotProduct Direction2d Direction2d where
    type DotProductResult Direction2d Direction2d = Float
    (Direction2d x1 y1) . (Direction2d x2 y2) =
        x1 * x2 + y1 * y2

x :: Direction2d coordinates
x =
    Direction2d 1.0 0.0

y :: Direction2d coordinates
y =
    Direction2d 0.0 1.0

determinant :: Direction2d coordinates -> Direction2d coordinates -> Float
determinant d1 d2 =
    let (Direction2d x1 y1) = d1
        (Direction2d x2 y2) = d2
     in x1 * y2 - y1 * x2
