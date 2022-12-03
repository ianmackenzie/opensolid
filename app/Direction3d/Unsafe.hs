module Direction3d.Unsafe (Direction3d (..)) where

import OpenSolid
import Vector3d.Type

data Direction3d coordinates = Direction3d !Float !Float !Float
    deriving (Eq, Show)

instance Negation (Direction3d coordinates) where
    negate (Direction3d x y z) = Direction3d (negate x) (negate y) (negate z)

instance DotProduct Direction3d Direction3d Float where
    (Direction3d x1 y1 z1) <> (Direction3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance DotProduct (Vector3d units) Direction3d (Qty units) where
    (Vector3d vx vy vz) <> (Direction3d dx dy dz) = vx * dx + vy * dy + vz * dz

instance DotProduct Direction3d (Vector3d units) (Qty units) where
    (Direction3d dx dy dz) <> (Vector3d vx vy vz) = dx * vx + dy * vy + dz * vz

instance Multiplication (Qty units) (Direction3d coordinates) (Vector3d units coordinates) where
    scale * (Direction3d x y z) = Vector3d (scale * x) (scale * y) (scale * z)

instance Multiplication (Direction3d coordinates) (Qty units) (Vector3d units coordinates) where
    (Direction3d x y z) * scale = Vector3d (x * scale) (y * scale) (z * scale)

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
