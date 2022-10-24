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

import qualified Length
import OpenSolid
import Point2d.Type
import qualified Vector2d
import Vector2d.Type

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
        px = Length.interpolateFrom x1 x2 t
        py = Length.interpolateFrom y1 y2 t
     in Point2d px py

midpoint :: Point2d coordinates -> Point2d coordinates -> Point2d coordinates
midpoint p1 p2 =
    let (Point2d x1 y1) = p1
        (Point2d x2 y2) = p2
        px = Length.midpoint x1 x2
        py = Length.midpoint y1 y2
     in Point2d px py

distanceFrom :: Point2d coordinates -> Point2d coordinates -> Length
distanceFrom p1 p2 =
    Vector2d.magnitude (Vector2d.from p1 p2)

translateBy :: Vector2d Length coordinates -> Point2d coordinates -> Point2d coordinates
translateBy vector point =
    let (Vector2d vx vy) = vector
        (Point2d px py) = point
     in Point2d (px + vx) (py + vy)
