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
    equalWithin,
) where

import Length qualified
import OpenSolid
import Qty qualified
import Vector2d (Vector2d (..))
import Vector2d qualified

data Point2d units coordinates = Point2d (Qty units) (Qty units)
    deriving (Eq)

deriving instance Show (Qty units) => Show (Point2d units coordinates)

instance Addition (Point2d units) (Vector2d units) (Point2d units) where
    (Point2d px py) + (Vector2d vx vy) = Point2d (px + vx) (py + vy)

instance Subtraction (Point2d units) (Vector2d units) (Point2d units) where
    (Point2d px py) - (Vector2d vx vy) = Point2d (px - vx) (py - vy)

instance Subtraction (Point2d units) (Point2d units) (Vector2d units) where
    (Point2d x1 y1) - (Point2d x2 y2) = Vector2d (x1 - x2) (y1 - y2)

origin :: Point2d units coordinates
origin = Point2d Qty.zero Qty.zero

x :: Qty units -> Point2d units coordinates
x px = Point2d px Qty.zero

y :: Qty units -> Point2d units coordinates
y py = Point2d Qty.zero py

xy :: Qty units -> Qty units -> Point2d units coordinates
xy = Point2d

meters :: Float -> Float -> Point2d Meters coordinates
meters px py = Point2d (Length.meters px) (Length.meters py)

interpolateFrom :: Point2d units coordinates -> Point2d units coordinates -> Float -> Point2d units coordinates
interpolateFrom (Point2d x1 y1) (Point2d x2 y2) t =
    Point2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Point2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
midpoint (Point2d x1 y1) (Point2d x2 y2) =
    Point2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

distanceFrom :: Point2d units coordinates -> Point2d units coordinates -> Qty units
distanceFrom p1 p2 = Vector2d.magnitude (p2 - p1)

translateBy :: Vector2d units coordinates -> Point2d units coordinates -> Point2d units coordinates
translateBy (Vector2d vx vy) (Point2d px py) = Point2d (px + vx) (py + vy)

equalWithin :: Qty units -> Point2d units coordinates -> Point2d units coordinates -> Bool
equalWithin tolerance p1 p2 = distanceFrom p1 p2 <= tolerance
