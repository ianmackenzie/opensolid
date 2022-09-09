module Direction3d (
    Direction3d,
    x,
    y,
    z,
) where

import qualified Area
import Direction3d.Unsafe
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String

instance Negation (Direction3d coordinates) where
    negate (Direction3d x y z) =
        Direction3d (- x) (- y) (- z)

instance DotProduct Direction3d Direction3d where
    type DotProductResult Direction3d Direction3d = Float
    (Direction3d x1 y1 z1) . (Direction3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

x :: Direction3d coordinates
x =
    Direction3d 1.0 0.0 0.0

y :: Direction3d coordinates
y =
    Direction3d 0.0 1.0 0.0

z :: Direction3d coordinates
z =
    Direction3d 0.0 0.0 1.0
