module Direction3d (
    Direction3d,
    x,
    y,
    z,
    components,
) where

import qualified Area
import qualified Length
import OpenSolid
import Quantity (Quantity)
import qualified Quantity
import qualified String
import {-# SOURCE #-} Vector3d (Vector3d)
import {-# SOURCE #-} qualified Vector3d

newtype Direction3d coordinates = Direction3d (Float, Float, Float)
    deriving (Eq)

instance Show (Direction3d coordinates) where
    show (Direction3d (x, y, z)) =
        let xString = String.fromFloat x
            yString = String.fromFloat y
            zString = String.fromFloat z
         in String.toList ("Direction3d (" ++ xString ++ ", " ++ yString ++ ", " ++ zString ++ ")")

instance Negation (Direction3d coordinates) where
    negate (Direction3d (x, y, z)) =
        Direction3d (- x, - y, - z)

instance Multiplication units Unitless => Multiplication (Quantity units) (Direction3d coordinates) where
    type Product (Quantity units) (Direction3d coordinates) = Vector3d (Product units Unitless) coordinates
    scale * (Direction3d (x, y, z)) =
        Vector3d.xyz (scale * x) (scale * y) (scale * z)

instance DotProduct Direction3d Direction3d where
    type DotProductResult Direction3d Direction3d = Float
    (Direction3d (x1, y1, z1)) . (Direction3d (x2, y2, z2)) =
        x1 * x2 + y1 * y2 + z1 * z2

instance CrossProduct Direction3d Direction3d where
    type CrossProductResult Direction3d Direction3d = Vector3d Unitless
    (Direction3d (x1, y1, z1)) >< (Direction3d (x2, y2, z2)) =
        Vector3d.xyz
            (y1 * z2 - z1 * y2)
            (z1 * x2 - x1 * z2)
            (x1 * y2 - y1 * x2)

x :: Direction3d coordinates
x =
    Direction3d (1.0, 0.0, 0.0)

y :: Direction3d coordinates
y =
    Direction3d (0.0, 1.0, 0.0)

z :: Direction3d coordinates
z =
    Direction3d (0.0, 0.0, 1.0)

components :: Direction3d coordinates -> (Float, Float, Float)
components (Direction3d components) =
    components
