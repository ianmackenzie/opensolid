module Point2d (
    Point2d (..),
    origin,
    meters,
) where

import Length (Length, Meters)
import qualified Length
import OpenSolid
import qualified String
import Vector2d (Vector2d (..))
import qualified Vector2d

data Point2d coordinates = Point2d Length Length
    deriving (Eq)

instance Show (Point2d coordinates) where
    show (Point2d x y) =
        let xString = String.fromFloat (Length.inMeters x)
            yString = String.fromFloat (Length.inMeters y)
         in String.toList ("Point2d.meters " ++ xString ++ " " ++ yString)

instance Addition Point2d (Vector2d Meters) where
    type Sum Point2d (Vector2d Meters) = Point2d
    (Point2d px py) + (Vector2d vx vy) =
        Point2d (px + vx) (py + vy)

instance Subtraction Point2d (Vector2d Meters) where
    type Difference Point2d (Vector2d Meters) = Point2d
    (Point2d px py) - (Vector2d vx vy) =
        Point2d (px - vx) (py - vy)

instance Subtraction Point2d Point2d where
    type Difference Point2d Point2d = Vector2d Meters
    (Point2d x1 y1) - (Point2d x2 y2) =
        Vector2d (x1 - x2) (y1 - y2)

origin :: Point2d coordinates
origin =
    Point2d zero zero

meters :: Float -> Float -> Point2d coordinates
meters x y =
    Point2d (Length.meters x) (Length.meters y)
