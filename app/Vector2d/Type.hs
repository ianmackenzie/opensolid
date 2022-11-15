module Vector2d.Type (Vector2d (..)) where

import OpenSolid

data Vector2d units coordinates = Vector2d !(Qty units) !(Qty units)
    deriving (Eq)

deriving instance Show (Qty units) => Show (Vector2d units coordinates)

instance Negation (Vector2d units coordinates) where
    negate (Vector2d vx vy) =
        Vector2d (negate vx) (negate vy)

instance Addition (Vector2d units) (Vector2d units) (Vector2d units) where
    (Vector2d x1 y1) + (Vector2d x2 y2) =
        Vector2d (x1 + x2) (y1 + y2)

instance Subtraction (Vector2d units) (Vector2d units) (Vector2d units) where
    (Vector2d x1 y1) - (Vector2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Vector2d units2 coordinates) (Vector2d units3 coordinates) where
    scale * (Vector2d vx vy) =
        Vector2d (scale * vx) (scale * vy)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector2d units1 coordinates) (Qty units2) (Vector2d units3 coordinates) where
    (Vector2d vx vy) * scale =
        Vector2d (vx * scale) (vy * scale)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Vector2d units1 coordinates) (Qty units2) (Vector2d units3 coordinates) where
    (Vector2d vx vy) / scale =
        Vector2d (vx / scale) (vy / scale)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector2d units1) (Vector2d units2) (Qty units3) where
    (Vector2d x1 y1) <> (Vector2d x2 y2) =
        x1 * x2 + y1 * y2
