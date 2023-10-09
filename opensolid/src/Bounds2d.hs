module Bounds2d
  ( Bounds2d (Bounds2d)
  , xCoordinate
  , yCoordinate
  , constant
  , hull2
  , hull3
  , hull4
  , aggregate2
  , intersects
  , intersection
  , interpolate
  )
where

import Bounds
import OpenSolid
import Point2d (Point2d (Point2d))
import Qty qualified
import Range (Range)
import Range qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Bounds2d nominal

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d :: Range units -> Range units -> Bounds2d (space @ units)

deriving instance Show (Bounds2d (space @ units))

instance IsBounds (Bounds2d (space @ units)) where
  aggregate2Impl = aggregate2
  intersectsImpl = intersects
  intersectionImpl = intersection

instance (units ~ units', space ~ space') => Subtraction (Point2d (space @ units)) (Bounds2d (space' @ units')) (VectorBounds2d (space @ units)) where
  Point2d px py - Bounds2d bx by = VectorBounds2d (px - bx) (py - by)

instance (units ~ units', space ~ space') => Subtraction (Bounds2d (space @ units)) (Point2d (space' @ units')) (VectorBounds2d (space @ units)) where
  Bounds2d bx by - Point2d px py = VectorBounds2d (bx - px) (by - py)

instance (units ~ units', space ~ space') => Subtraction (Bounds2d (space @ units)) (Bounds2d (space' @ units')) (VectorBounds2d (space @ units)) where
  Bounds2d x1 y1 - Bounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

xCoordinate :: Bounds2d (space @ units) -> Range units
xCoordinate (Bounds2d x _) = x

yCoordinate :: Bounds2d (space @ units) -> Range units
yCoordinate (Bounds2d _ y) = y

constant :: Point2d (space @ units) -> Bounds2d (space @ units)
constant (Point2d x y) =
  Bounds2d (Range.constant x) (Range.constant y)

aggregate2 :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bounds2d (space @ units)
aggregate2 (Bounds2d x1 y1) (Bounds2d x2 y2) =
  Bounds2d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2)

intersects :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
intersects (Bounds2d x1 y1) (Bounds2d x2 y2) =
  Range.intersects x1 x2 && Range.intersects y1 y2

intersection :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Maybe (Bounds2d (space @ units))
intersection (Bounds2d x1 y1) (Bounds2d x2 y2) = do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  Just (Bounds2d x y)

hull2 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull2 (Point2d x1 y1) (Point2d x2 y2) =
  Bounds2d (Range.from x1 x2) (Range.from y1 y2)

hull3 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull3 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) =
  let minX = Qty.min (Qty.min x1 x2) x3
      maxX = Qty.max (Qty.max x1 x2) x3
      minY = Qty.min (Qty.min y1 y2) y3
      maxY = Qty.max (Qty.max y1 y2) y3
   in Bounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull4 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4) =
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
      maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
      minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
      maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
   in Bounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

interpolate :: Bounds2d (space @ units) -> Float -> Float -> Point2d (space @ units)
interpolate (Bounds2d x y) u v =
  Point2d (Range.interpolate x u) (Range.interpolate y v)
