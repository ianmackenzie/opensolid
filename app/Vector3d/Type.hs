module Vector3d.Type (Vector3d (..)) where

import OpenSolid

data Vector3d scalar coordinates = Vector3d !scalar !scalar !scalar
    deriving (Eq, Show)

instance Scalar scalar => Negation (Vector3d scalar coordinates) where
    negate (Vector3d vx vy vz) =
        Vector3d (negate vx) (negate vy) (negate vz)

instance Scalar scalar => Addition (Vector3d scalar coordinates) where
    (Vector3d x1 y1 z1) + (Vector3d x2 y2 z2) =
        Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Scalar scalar => Subtraction (Vector3d scalar coordinates) where
    (Vector3d x1 y1 z1) - (Vector3d x2 y2 z2) =
        Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Scalar scalar => Multiplication Float (Vector3d scalar coordinates) (Vector3d scalar coordinates) where
    scale * (Vector3d vx vy vz) =
        Vector3d (scale * vx) (scale * vy) (scale * vz)

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Quantity units) (Vector3d scalar coordinates) (Vector3d result coordinates) where
    scale * (Vector3d vx vy vz) =
        Vector3d (scale * vx) (scale * vy) (scale * vz)

instance Scalar scalar => Multiplication (Vector3d scalar coordinates) Float (Vector3d scalar coordinates) where
    (Vector3d vx vy vz) * scale =
        Vector3d (scale * vx) (scale * vy) (scale * vz)

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Vector3d scalar coordinates) (Quantity units) (Vector3d result coordinates) where
    (Vector3d vx vy vz) * scale =
        Vector3d (scale * vx) (scale * vy) (scale * vz)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Division scalar1 scalar2 result) => Division (Vector3d scalar1 coordinates) scalar2 (Vector3d result coordinates) where
    (Vector3d vx vy vz) / scale =
        Vector3d (vx / scale) (vy / scale) (vz / scale)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => DotProduct (Vector3d scalar1) (Vector3d scalar2) result where
    (Vector3d x1 y1 z1) . (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => CrossProduct (Vector3d scalar1) (Vector3d scalar2) (Vector3d result) where
    (Vector3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz
