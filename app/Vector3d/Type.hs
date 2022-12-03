module Vector3d.Type (Vector3d (..)) where

import OpenSolid

data Vector3d units coordinates = Vector3d !(Qty units) !(Qty units) !(Qty units)
    deriving (Eq)

deriving instance Show (Qty units) => Show (Vector3d units coordinates)

instance Negation (Vector3d units coordinates) where
    negate (Vector3d vx vy vz) = Vector3d (negate vx) (negate vy) (negate vz)

instance Addition (Vector3d units) (Vector3d units) (Vector3d units) where
    (Vector3d x1 y1 z1) + (Vector3d x2 y2 z2) = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance Subtraction (Vector3d units) (Vector3d units) (Vector3d units) where
    (Vector3d x1 y1 z1) - (Vector3d x2 y2 z2) = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Vector3d units2 coordinates) (Vector3d units3 coordinates) where
    scale * (Vector3d vx vy vz) = Vector3d (scale * vx) (scale * vy) (scale * vz)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector3d units1 coordinates) (Qty units2) (Vector3d units3 coordinates) where
    (Vector3d vx vy vz) * scale = Vector3d (vx * scale) (vy * scale) (vz * scale)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Vector3d units1 coordinates) (Qty units2) (Vector3d units3 coordinates) where
    (Vector3d vx vy vz) / scale = Vector3d (vx / scale) (vy / scale) (vz / scale)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector3d units1) (Vector3d units2) (Qty units3) where
    (Vector3d x1 y1 z1) <> (Vector3d x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (Vector3d units1) (Vector3d units2) (Vector3d units3) where
    (Vector3d x1 y1 z1) >< (Vector3d x2 y2 z2) =
        Vector3d
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)
