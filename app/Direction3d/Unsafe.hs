module Direction3d.Unsafe (Direction3d (..)) where

import OpenSolid
import Vector3d.Type

data Direction3d coordinates = Direction3d !Float !Float !Float
    deriving (Eq, Show)

instance Negation (Direction3d coordinates) where
    negate (Direction3d x y z) =
        Direction3d (negate x) (negate y) (negate z)

instance DotProduct Direction3d Direction3d Float where
    (Direction3d x1 y1 z1) <> (Direction3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Scalar scalar => DotProduct (Vector3d scalar) Direction3d scalar where
    (Vector3d vx vy vz) <> (Direction3d dx dy dz) =
        vx * dx + vy * dy + vz * dz

instance Scalar scalar => DotProduct Direction3d (Vector3d scalar) scalar where
    (Direction3d dx dy dz) <> (Vector3d vx vy vz) =
        dx * vx + dy * vy + dz * vz

instance Scalar scalar => Multiplication scalar (Direction3d coordinates) (Vector3d scalar coordinates) where
    scale * (Direction3d x y z) =
        Vector3d (scale * x) (scale * y) (scale * z)

instance Scalar scalar => Multiplication (Direction3d coordinates) scalar (Vector3d scalar coordinates) where
    (Direction3d x y z) * scale =
        Vector3d (x * scale) (y * scale) (z * scale)

instance Scalar scalar => CrossProduct (Vector3d scalar) Direction3d (Vector3d scalar) where
    (Vector3d x1 y1 z1) >< (Direction3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz

instance Scalar scalar => CrossProduct Direction3d (Vector3d scalar) (Vector3d scalar) where
    (Direction3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz

instance CrossProduct Direction3d Direction3d (Vector3d Float) where
    (Direction3d x1 y1 z1) >< (Direction3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)
