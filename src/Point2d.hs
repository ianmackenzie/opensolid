module Point2d (
    Point2d (..),
    origin,
    x,
    y,
    xy,
    meters,
    midpoint,
    interpolateFrom,
    distanceFrom,
    translateBy,
) where

import Length qualified
import OpenSolid
import Qty qualified
import Vector2d (Vector2d (..))
import Vector2d qualified

data Point2d coordinates = Point2d Length Length
    deriving (Eq, Show)

instance Addition Point2d (Vector2d Meters) Point2d where
    (Point2d px py) + (Vector2d vx vy) = Point2d (px + vx) (py + vy)

instance Subtraction Point2d (Vector2d Meters) Point2d where
    (Point2d px py) - (Vector2d vx vy) = Point2d (px - vx) (py - vy)

instance Subtraction Point2d Point2d (Vector2d Meters) where
    (Point2d x1 y1) - (Point2d x2 y2) = Vector2d (x1 - x2) (y1 - y2)

instance ApproximateEquality (Point2d coordinates) Meters where
    p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

origin :: Point2d coordinates
origin = Point2d Qty.zero Qty.zero

x :: Length -> Point2d coordinates
x px = Point2d px Qty.zero

y :: Length -> Point2d coordinates
y py = Point2d Qty.zero py

xy :: Length -> Length -> Point2d coordinates
xy = Point2d

meters :: Float -> Float -> Point2d coordinates
meters px py = Point2d (Length.meters px) (Length.meters py)

interpolateFrom :: Point2d coordinates -> Point2d coordinates -> Float -> Point2d coordinates
interpolateFrom (Point2d x1 y1) (Point2d x2 y2) t =
    Point2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Point2d coordinates -> Point2d coordinates -> Point2d coordinates
midpoint (Point2d x1 y1) (Point2d x2 y2) =
    Point2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

distanceFrom :: Point2d coordinates -> Point2d coordinates -> Length
distanceFrom p1 p2 = Vector2d.magnitude (p2 - p1)

translateBy :: Vector2d Meters coordinates -> Point2d coordinates -> Point2d coordinates
translateBy (Vector2d vx vy) (Point2d px py) = Point2d (px + vx) (py + vy)
