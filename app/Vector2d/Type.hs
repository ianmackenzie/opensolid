module Vector2d.Type (Vector2d (..)) where

import OpenSolid

data Vector2d qty coordinates = Vector2d !qty !qty
    deriving (Eq, Show)

instance Negation (Vector2d (Qty a) coordinates) where
    negate (Vector2d vx vy) =
        Vector2d (negate vx) (negate vy)

instance Addition (Vector2d (Qty a)) (Vector2d (Qty a)) (Vector2d (Qty a)) coordinates where
    (Vector2d x1 y1) + (Vector2d x2 y2) =
        Vector2d (x1 + x2) (y1 + y2)

instance Subtraction (Vector2d (Qty a)) (Vector2d (Qty a)) (Vector2d (Qty a)) coordinates where
    (Vector2d x1 y1) - (Vector2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Qty a) (Vector2d (Qty b) coordinates) (Vector2d (Qty c) coordinates) where
    scale * (Vector2d vx vy) =
        Vector2d (scale * vx) (scale * vy)

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Vector2d (Qty a) coordinates) (Qty b) (Vector2d (Qty c) coordinates) where
    (Vector2d vx vy) * scale =
        Vector2d (vx * scale) (vy * scale)

instance Division (Qty a) (Qty b) (Qty c) => Division (Vector2d (Qty a) coordinates) (Qty b) (Vector2d (Qty c) coordinates) where
    (Vector2d vx vy) / scale =
        Vector2d (vx / scale) (vy / scale)

instance Multiplication (Qty a) (Qty b) (Qty c) => DotProduct (Vector2d (Qty a)) (Vector2d (Qty b)) (Qty c) coordinates where
    (Vector2d x1 y1) <> (Vector2d x2 y2) =
        x1 * x2 + y1 * y2
