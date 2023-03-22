module BoundingBox2d
  ( BoundingBox2d (..)
  , constant
  , hull2
  , hull3
  , hull4
  , aggregate
  , overlaps
  , interpolate
  )
where

import Bounds
import CoordinateSystem (Units)
import OpenSolid
import Point2d (Point2d (..))
import Range (Range)
import Range qualified
import VectorBox2d (VectorBox2d (VectorBox2d))

type role BoundingBox2d nominal

data BoundingBox2d (coordinateSystem :: CoordinateSystem)
  = BoundingBox2d
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))
  deriving (Show)

instance Bounds (BoundingBox2d (Coordinates space units)) where
  aggregate (BoundingBox2d x1 y1) (BoundingBox2d x2 y2) =
    BoundingBox2d (Range.aggregate x1 x2) (Range.aggregate y1 y2)

  overlaps (BoundingBox2d x1 y1) (BoundingBox2d x2 y2) =
    Range.overlaps x1 x2 && Range.overlaps y1 y2

instance (units ~ units', space ~ space') => Subtraction (Point2d (Coordinates space units)) (BoundingBox2d (Coordinates space' units')) (VectorBox2d (Coordinates space units)) where
  Point2d px py - BoundingBox2d bx by = VectorBox2d (px - bx) (py - by)

instance (units ~ units', space ~ space') => Subtraction (BoundingBox2d (Coordinates space units)) (Point2d (Coordinates space' units')) (VectorBox2d (Coordinates space units)) where
  BoundingBox2d bx by - Point2d px py = VectorBox2d (bx - px) (by - py)

constant :: Point2d (Coordinates space units) -> BoundingBox2d (Coordinates space units)
constant (Point2d x y) =
  BoundingBox2d (Range.constant x) (Range.constant y)

hull2
  :: Point2d (Coordinates space units)
  -> Point2d (Coordinates space units)
  -> BoundingBox2d (Coordinates space units)
hull2 (Point2d x1 y1) (Point2d x2 y2) =
  BoundingBox2d (Range.from x1 x2) (Range.from y1 y2)

hull3
  :: Point2d (Coordinates space units)
  -> Point2d (Coordinates space units)
  -> Point2d (Coordinates space units)
  -> BoundingBox2d (Coordinates space units)
hull3 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) =
  let minX = min (min x1 x2) x3
      maxX = max (max x1 x2) x3
      minY = min (min y1 y2) y3
      maxY = max (max y1 y2) y3
   in BoundingBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4
  :: Point2d (Coordinates space units)
  -> Point2d (Coordinates space units)
  -> Point2d (Coordinates space units)
  -> Point2d (Coordinates space units)
  -> BoundingBox2d (Coordinates space units)
hull4 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4) =
  let minX = min (min (min x1 x2) x3) x4
      maxX = max (max (max x1 x2) x3) x4
      minY = min (min (min y1 y2) y3) y4
      maxY = max (max (max y1 y2) y3) y4
   in BoundingBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

interpolate :: BoundingBox2d (Coordinates space units) -> Float -> Float -> Point2d (Coordinates space units)
interpolate (BoundingBox2d x y) u v =
  Point2d (Range.interpolate x u) (Range.interpolate y v)
