module Vector2d.Type (Vector2d (..)) where

import OpenSolid

data Vector2d scalar coordinates = Vector2d !scalar !scalar
    deriving (Eq, Show)

instance Scalar scalar => Negation (Vector2d scalar coordinates) where
    negate (Vector2d vx vy) =
        Vector2d (negate vx) (negate vy)

instance Scalar scalar => Addition (Vector2d scalar coordinates) where
    (Vector2d x1 y1) + (Vector2d x2 y2) =
        Vector2d (x1 + x2) (y1 + y2)

instance Scalar scalar => Subtraction (Vector2d scalar coordinates) where
    (Vector2d x1 y1) - (Vector2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

instance Scalar scalar => Multiplication Float (Vector2d scalar coordinates) (Vector2d scalar coordinates) where
    scale * (Vector2d vx vy) =
        Vector2d (scale * vx) (scale * vy)

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Quantity units) (Vector2d scalar coordinates) (Vector2d result coordinates) where
    scale * (Vector2d vx vy) =
        Vector2d (scale * vx) (scale * vy)

instance Scalar scalar => Multiplication (Vector2d scalar coordinates) Float (Vector2d scalar coordinates) where
    (Vector2d vx vy) * scale =
        Vector2d (scale * vx) (scale * vy)

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Vector2d scalar coordinates) (Quantity units) (Vector2d result coordinates) where
    (Vector2d vx vy) * scale =
        Vector2d (scale * vx) (scale * vy)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Division scalar1 scalar2 result) => Division (Vector2d scalar1 coordinates) scalar2 (Vector2d result coordinates) where
    (Vector2d vx vy) / scale =
        Vector2d (vx / scale) (vy / scale)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => DotProduct (Vector2d scalar1) (Vector2d scalar2) result where
    (Vector2d x1 y1) . (Vector2d x2 y2) =
        x1 * x2 + y1 * y2
