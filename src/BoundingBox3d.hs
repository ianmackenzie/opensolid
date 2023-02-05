module BoundingBox3d
  ( BoundingBox3d (..)
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
import OpenSolid
import Point3d (Point3d (..))
import Range (Range)
import Range qualified
import VectorBox3d (VectorBox3d (VectorBox3d))

type role BoundingBox3d nominal nominal

type BoundingBox3d :: Type -> Type -> Type
data BoundingBox3d coordinates units = BoundingBox3d (Range units) (Range units) (Range units) deriving (Show)

instance Bounds (BoundingBox3d coordinates units) where
  aggregate (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
    BoundingBox3d (Range.aggregate x1 x2) (Range.aggregate y1 y2) (Range.aggregate z1 z2)

  overlaps (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
    Range.overlaps x1 x2 && Range.overlaps y1 y2 && Range.overlaps z1 z2

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point3d coordinates units) (BoundingBox3d coordinates' units') (VectorBox3d coordinates units) where
  Point3d px py pz - BoundingBox3d bx by bz = VectorBox3d (px - bx) (py - by) (pz - bz)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (BoundingBox3d coordinates units) (Point3d coordinates' units') (VectorBox3d coordinates units) where
  BoundingBox3d bx by bz - Point3d px py pz = VectorBox3d (bx - px) (by - py) (bz - pz)

constant :: Point3d coordinates units -> BoundingBox3d coordinates units
constant (Point3d x y z) =
  BoundingBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Point3d coordinates units -> Point3d coordinates units -> BoundingBox3d coordinates units
hull2 (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  BoundingBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 :: Point3d coordinates units -> Point3d coordinates units -> Point3d coordinates units -> BoundingBox3d coordinates units
hull3 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) =
  let minX = min (min x1 x2) x3
      maxX = max (max x1 x2) x3
      minY = min (min y1 y2) y3
      maxY = max (max y1 y2) y3
      minZ = min (min z1 z2) z3
      maxZ = max (max z1 z2) z3
   in BoundingBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hull4 :: Point3d coordinates units -> Point3d coordinates units -> Point3d coordinates units -> Point3d coordinates units -> BoundingBox3d coordinates units
hull4 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) (Point3d x4 y4 z4) =
  let minX = min (min (min x1 x2) x3) x4
      maxX = max (max (max x1 x2) x3) x4
      minY = min (min (min y1 y2) y3) y4
      maxY = max (max (max y1 y2) y3) y4
      minZ = min (min (min z1 z2) z3) z4
      maxZ = max (max (max z1 z2) z3) z4
   in BoundingBox3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

interpolate :: BoundingBox3d coordinates units -> Float -> Float -> Float -> Point3d coordinates units
interpolate (BoundingBox3d x y z) u v w =
  Point3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)
