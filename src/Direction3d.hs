module Direction3d (
    Direction3d (Direction3d),
    unsafe,
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

import OpenSolid
import Vector3d (Vector3d (..))

data Direction3d coordinates = Unsafe Float Float Float
    deriving (Eq, Show)

{-# COMPLETE Direction3d #-}

pattern Direction3d :: Float -> Float -> Float -> Direction3d coordinates
pattern Direction3d x y z = Unsafe x y z

instance Negation (Direction3d coordinates) where
    negate (Direction3d dx dy dz) = unsafe (negate dx) (negate dy) (negate dz)

instance DotProduct Direction3d Direction3d Float where
    (Direction3d x1 y1 z1) <> (Direction3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance DotProduct (Vector3d units) Direction3d (Qty units) where
    (Vector3d vx vy vz) <> (Direction3d dx dy dz) = vx * dx + vy * dy + vz * dz

instance DotProduct Direction3d (Vector3d units) (Qty units) where
    (Direction3d dx dy dz) <> (Vector3d vx vy vz) = dx * vx + dy * vy + dz * vz

instance Multiplication (Qty units) (Direction3d coordinates) (Vector3d units coordinates) where
    scale * (Direction3d dx dy dz) = Vector3d (scale * dx) (scale * dy) (scale * dz)

instance Multiplication (Direction3d coordinates) (Qty units) (Vector3d units coordinates) where
    (Direction3d dx dy dz) * scale = Vector3d (dx * scale) (dy * scale) (dz * scale)

instance CrossProduct (Vector3d units) Direction3d (Vector3d units) where
    (Vector3d x1 y1 z1) >< (Direction3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

instance CrossProduct Direction3d (Vector3d units) (Vector3d units) where
    (Direction3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

instance CrossProduct Direction3d Direction3d (Vector3d Unitless) where
    (Direction3d x1 y1 z1) >< (Direction3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

unsafe :: Float -> Float -> Float -> Direction3d coordinates
unsafe = Unsafe

positiveX :: Direction3d coordinates
positiveX = unsafe 1.0 0.0 0.0

negativeX :: Direction3d coordinates
negativeX = negate positiveX

positiveY :: Direction3d coordinates
positiveY = unsafe 0.0 1.0 0.0

negativeY :: Direction3d coordinates
negativeY = negate positiveY

positiveZ :: Direction3d coordinates
positiveZ = unsafe 0.0 0.0 1.0

negativeZ :: Direction3d coordinates
negativeZ = negate positiveZ

x :: Direction3d coordinates
x = positiveX

y :: Direction3d coordinates
y = positiveY

z :: Direction3d coordinates
z = positiveZ
