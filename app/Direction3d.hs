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

import Direction3d.Unsafe
import OpenSolid

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
