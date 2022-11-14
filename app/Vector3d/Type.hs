module Vector3d.Type (Vector3d (..)) where

import OpenSolid

data Vector3d qty coordinates = Vector3d !qty !qty !qty
    deriving (Eq, Show)

instance Negation (Vector3d (Qty a) coordinates) where
    negate (Vector3d vx vy vz) =
        Vector3d (negate vx) (negate vy) (negate vz)

instance Addition (Vector3d (Qty a)) (Vector3d (Qty a)) (Vector3d (Qty a)) coordinates where
    (Vector3d x1 y1 z1) + (Vector3d x2 y2 z2) =
        Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (Vector3d (Qty a)) (Vector3d (Qty a)) (Vector3d (Qty a)) coordinates where
    (Vector3d x1 y1 z1) - (Vector3d x2 y2 z2) =
        Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Qty a) (Vector3d (Qty b) coordinates) (Vector3d (Qty c) coordinates) where
    scale * (Vector3d vx vy vz) =
        Vector3d (scale * vx) (scale * vy) (scale * vz)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Vector3d (Qty a) coordinates) (Qty b) (Vector3d (Qty c) coordinates) where
    (Vector3d vx vy vz) * scale =
        Vector3d (vx * scale) (vy * scale) (vz * scale)

instance Division (Qty a) (Qty b) (Qty c) => Division (Vector3d (Qty a) coordinates) (Qty b) (Vector3d (Qty c) coordinates) where
    (Vector3d vx vy vz) / scale =
        Vector3d (vx / scale) (vy / scale) (vz / scale)

instance Multiplication (Qty a) (Qty b) (Qty c) => DotProduct (Vector3d (Qty a)) (Vector3d (Qty b)) (Qty c) coordinates where
    (Vector3d x1 y1 z1) <> (Vector3d x2 y2 z2) =
        x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty a) (Qty b) (Qty c) => CrossProduct (Vector3d (Qty a)) (Vector3d (Qty b)) (Vector3d (Qty c)) coordinates where
    (Vector3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        let vx = y1 * z2 - z1 * y2
            vy = z1 * x2 - x1 * z2
            vz = x1 * y2 - y1 * x2
         in Vector3d vx vy vz
