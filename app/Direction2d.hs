module Direction2d (
    Direction2d,
    x,
    y,
    components,
) where

import qualified Area
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String
import {-# SOURCE #-} Vector2d (Vector2d)
import {-# SOURCE #-} qualified Vector2d

newtype Direction2d coordinates = Direction2d (Float, Float)
    deriving (Eq)

instance Show (Direction2d coordinates) where
    show (Direction2d (x, y)) =
        let xString = String.fromFloat x
            yString = String.fromFloat y
         in String.toList ("Direction2d (" ++ xString ++ ", " ++ yString ++ ")")

instance Negation (Direction2d coordinates) where
    negate (Direction2d (x, y)) =
        Direction2d (- x, - y)

instance Multiplication units Unitless => Multiplication (Quantity units) (Direction2d coordinates) where
    type Product (Quantity units) (Direction2d coordinates) = Vector2d (Product units Unitless) coordinates
    scale * (Direction2d (x, y)) =
        Vector2d.xy (scale * x) (scale * y)

instance DotProduct Direction2d Direction2d where
    type DotProductResult Direction2d Direction2d = Float
    (Direction2d (x1, y1)) . (Direction2d (x2, y2)) =
        x1 * x2 + y1 * y2

x :: Direction2d coordinates
x =
    Direction2d (1.0, 0.0)

y :: Direction2d coordinates
y =
    Direction2d (0.0, 1.0)

components :: Direction2d coordinates -> (Float, Float)
components (Direction2d components) =
    components

determinant :: Direction2d coordinates -> Direction2d coordinates -> Float
determinant d1 d2 =
    let (x1, y1) = components d1
        (x2, y2) = components d2
     in x1 * y2 - y1 * x2
