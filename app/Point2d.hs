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
) where

import Length (Length, Meters)
import qualified Length
import OpenSolid
import qualified Quantity
import qualified Show
import qualified String
import Vector2d (Vector2d (..))
import qualified Vector2d

data Point2d coordinates = Point2d Length Length
    deriving (Eq)

instance Show (Point2d coordinates) where
    showsPrec precedence (Point2d x y) =
        Show.primitive precedence "Point2d.meters" [Length.inMeters x, Length.inMeters y]

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
    Point2d Length.zero Length.zero

x :: Length -> Point2d coordinates
x x =
    Point2d x Length.zero

y :: Length -> Point2d coordinates
y y =
    Point2d Length.zero y

xy :: Length -> Length -> Point2d coordinates
xy =
    Point2d

meters :: Float -> Float -> Point2d coordinates
meters x y =
    Point2d (Length.meters x) (Length.meters y)

interpolateFrom :: Point2d coordinates -> Point2d coordinates -> Float -> Point2d coordinates
interpolateFrom p1 p2 t =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
        x = Quantity.interpolateFrom x1 x2 t
        y = Quantity.interpolateFrom y1 y2 t
     in Point2d x y

midpoint :: Point2d coordinates -> Point2d coordinates -> Point2d coordinates
midpoint p1 p2 =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
        x = Quantity.midpoint x1 x2
        y = Quantity.midpoint y1 y2
     in Point2d x y

distanceFrom :: Point2d coordinates -> Point2d coordinates -> Length
distanceFrom p1 p2 =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
     in Quantity.hypot2 (x2 - x1) (y2 - y1)
