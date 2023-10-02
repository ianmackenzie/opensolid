module BoundingBox3d
  ( BoundingBox3d (BoundingBox3d)
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
import Point3d (Point3d (Point3d))
import Qty qualified
import Range (Range)
import Range qualified
import VectorBox3d (VectorBox3d (VectorBox3d))

type role BoundingBox3d nominal

data BoundingBox3d (coordinateSystem :: CoordinateSystem) where
  BoundingBox3d :: Range units -> Range units -> Range units -> BoundingBox3d (space @ units)

deriving instance Show (BoundingBox3d (space @ units))

instance IsBounds (BoundingBox3d (space @ units)) where
  aggregate2Impl = aggregate2
  intersectsImpl = intersects
  intersectionImpl = intersection

instance (units ~ units', space ~ space') => Subtraction (Point3d (space @ units)) (BoundingBox3d (space' @ units')) (VectorBox3d (space @ units)) where
  Point3d px py pz - BoundingBox3d bx by bz = VectorBox3d (px - bx) (py - by) (pz - bz)

instance (units ~ units', space ~ space') => Subtraction (BoundingBox3d (space @ units)) (Point3d (space' @ units')) (VectorBox3d (space @ units)) where
  BoundingBox3d bx by bz - Point3d px py pz = VectorBox3d (bx - px) (by - py) (bz - pz)

instance (units ~ units', space ~ space') => Subtraction (BoundingBox3d (space @ units)) (BoundingBox3d (space' @ units')) (VectorBox3d (space @ units)) where
  BoundingBox3d x1 y1 z1 - BoundingBox3d x2 y2 z2 = VectorBox3d (x1 - x2) (y1 - y2) (z1 - z2)

constant :: Point3d (space @ units) -> BoundingBox3d (space @ units)
constant (Point3d x y z) =
  BoundingBox3d (Range.constant x) (Range.constant y) (Range.constant z)

aggregate2 :: BoundingBox3d (space @ units) -> BoundingBox3d (space @ units) -> BoundingBox3d (space @ units)
aggregate2 (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
  BoundingBox3d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2) (Range.aggregate2 z1 z2)

intersects :: BoundingBox3d (space @ units) -> BoundingBox3d (space @ units) -> Bool
intersects (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
  Range.intersects x1 x2 && Range.intersects y1 y2 && Range.intersects z1 z2

intersection :: BoundingBox3d (space @ units) -> BoundingBox3d (space @ units) -> Maybe (BoundingBox3d (space @ units))
intersection (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) = do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  z <- Range.intersection z1 z2
  Just (BoundingBox3d x y z)

hull2 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  BoundingBox3d (space @ units)
hull2 (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  BoundingBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  BoundingBox3d (space @ units)
hull3 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) =
  let minX = Qty.min (Qty.min x1 x2) x3
      maxX = Qty.max (Qty.max x1 x2) x3
      minY = Qty.min (Qty.min y1 y2) y3
      maxY = Qty.max (Qty.max y1 y2) y3
      minZ = Qty.min (Qty.min z1 z2) z3
      maxZ = Qty.max (Qty.max z1 z2) z3
   in BoundingBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hull4 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  BoundingBox3d (space @ units)
hull4 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) (Point3d x4 y4 z4) =
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
      maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
      minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
      maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
      minZ = Qty.min (Qty.min (Qty.min z1 z2) z3) z4
      maxZ = Qty.max (Qty.max (Qty.max z1 z2) z3) z4
   in BoundingBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

interpolate :: BoundingBox3d (space @ units) -> Float -> Float -> Float -> Point3d (space @ units)
interpolate (BoundingBox3d x y z) u v w =
  Point3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)
