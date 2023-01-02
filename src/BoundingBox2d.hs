module BoundingBox2d (
    BoundingBox2d (..),
    constant,
    hull2,
    hull3,
    hull4,
    aggregate,
    overlaps,
) where

import Bounds
import OpenSolid
import Point2d (Point2d (..))
import Range (Range)
import Range qualified

data BoundingBox2d coordinates = BoundingBox2d (Range Meters) (Range Meters)

instance Bounds (BoundingBox2d coordinates) where
    aggregate (BoundingBox2d x1 y1) (BoundingBox2d x2 y2) =
        BoundingBox2d (Range.aggregate x1 x2) (Range.aggregate y1 y2)

    overlaps (BoundingBox2d x1 y1) (BoundingBox2d x2 y2) =
        Range.overlaps x1 x2 && Range.overlaps y1 y2

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
