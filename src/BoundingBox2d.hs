module BoundingBox2d (
    BoundingBox2d (..),
    constant,
    hull2,
    hull3,
    hull4,
    aggregate,
    overlaps,
    interpolate,
) where

import Bounds
import OpenSolid
import Point2d (Point2d (..))
import Range (Range)
import Range qualified
import VectorBox2d (VectorBox2d (VectorBox2d))

data BoundingBox2d coordinates = BoundingBox2d (Range Meters) (Range Meters)
    deriving (Show)

instance Bounds (BoundingBox2d coordinates) where
    aggregate (BoundingBox2d x1 y1) (BoundingBox2d x2 y2) =
        BoundingBox2d (Range.aggregate x1 x2) (Range.aggregate y1 y2)

    overlaps (BoundingBox2d x1 y1) (BoundingBox2d x2 y2) =
        Range.overlaps x1 x2 && Range.overlaps y1 y2

instance Subtraction Point2d BoundingBox2d (VectorBox2d Meters) where
    (Point2d px py) - (BoundingBox2d bx by) = VectorBox2d (px - bx) (py - by)

instance Subtraction BoundingBox2d Point2d (VectorBox2d Meters) where
    (BoundingBox2d bx by) - (Point2d px py) = VectorBox2d (bx - px) (by - py)

constant :: Point2d coordinates -> BoundingBox2d coordinates
constant (Point2d x y) =
    BoundingBox2d (Range.constant x) (Range.constant y)

hull2 :: Point2d coordinates -> Point2d coordinates -> BoundingBox2d coordinates
hull2 (Point2d x1 y1) (Point2d x2 y2) =
    BoundingBox2d (Range.from x1 x2) (Range.from y1 y2)

hull3 :: Point2d coordinates -> Point2d coordinates -> Point2d coordinates -> BoundingBox2d coordinates
hull3 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) =
    let minX = min (min x1 x2) x3
        maxX = max (max x1 x2) x3
        minY = min (min y1 y2) y3
        maxY = max (max y1 y2) y3
     in BoundingBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 :: Point2d coordinates -> Point2d coordinates -> Point2d coordinates -> Point2d coordinates -> BoundingBox2d coordinates
hull4 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4) =
    let minX = min (min (min x1 x2) x3) x4
        maxX = max (max (max x1 x2) x3) x4
        minY = min (min (min y1 y2) y3) y4
        maxY = max (max (max y1 y2) y3) y4
     in BoundingBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

interpolate :: BoundingBox2d coordinates -> Float -> Float -> Point2d coordinates
interpolate (BoundingBox2d x y) u v =
    Point2d (Range.interpolate x u) (Range.interpolate y v)
