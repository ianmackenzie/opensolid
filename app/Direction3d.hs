module Direction3d (
    Direction3d,
    x,
    y,
    z,
    positiveX,
    negativeX,
    positiveY,
    negativeY,
    positiveZ,
    negativeZ,
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
        Direction3d (negate x) (negate y) (negate z)

instance DotProduct Direction3d Direction3d where
    type DotProductResult Direction3d Direction3d = Float
    (Direction3d x1 y1 z1) . (Direction3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

positiveX :: Direction3d coordinates
positiveX =
    Direction3d 1.0 0.0 0.0

negativeX :: Direction3d coordinates
negativeX =
    negate positiveX

positiveY :: Direction3d coordinates
positiveY =
    Direction3d 0.0 1.0 0.0

negativeY :: Direction3d coordinates
negativeY =
    negate positiveY

positiveZ :: Direction3d coordinates
positiveZ =
    Direction3d 0.0 0.0 1.0

negativeZ :: Direction3d coordinates
negativeZ =
    negate positiveZ

x :: Direction3d coordinates
x =
    positiveX

y :: Direction3d coordinates
y =
    positiveY

z :: Direction3d coordinates
z =
    positiveZ
