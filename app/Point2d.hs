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
import qualified Vector2d
import Vector2d.Type

data Point2d coordinates = Point2d !Length !Length
    deriving (Eq)

instance Show (Point2d coordinates) where
    showsPrec precedence (Point2d px py) =
        Show.primitive precedence "Point2d.meters" [Length.inMeters px, Length.inMeters py]

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
x px =
    Point2d px Length.zero

y :: Length -> Point2d coordinates
y py =
    Point2d Length.zero py

xy :: Length -> Length -> Point2d coordinates
xy =
    Point2d

meters :: Float -> Float -> Point2d coordinates
meters px py =
    Point2d (Length.meters px) (Length.meters py)

interpolateFrom :: Point2d coordinates -> Point2d coordinates -> Float -> Point2d coordinates
interpolateFrom p1 p2 t =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
        px = Quantity.interpolateFrom x1 x2 t
        py = Quantity.interpolateFrom y1 y2 t
     in Point2d px py

midpoint :: Point2d coordinates -> Point2d coordinates -> Point2d coordinates
midpoint p1 p2 =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
        px = Quantity.midpoint x1 x2
        py = Quantity.midpoint y1 y2
     in Point2d px py

distanceFrom :: Point2d coordinates -> Point2d coordinates -> Length
distanceFrom p1 p2 =
    Vector2d.magnitude (p2 - p1)
