module Bounds3d
  ( Bounds3d
  , xRange
  , yRange
  , zRange
  , xyzRanges
  , xyz
  , constant
  , hull2
  , hull3
  , hull4
  , aggregate2
  , exclusion
  , inclusion
  , contains
  , isContainedIn
  , separation
  , overlap
  , intersection
  , interpolate
  )
where

import Bounds qualified
import Data.Coerce qualified
import Maybe qualified
import OpenSolid
import Point3d (Point3d (Point3d))
import Qty qualified
import Range (Range)
import Range qualified
import Units qualified
import VectorBounds3d (VectorBounds3d (VectorBounds3d))

type role Bounds3d phantom

data Bounds3d (coordinateSystem :: CoordinateSystem) where
  Bounds3d ::
    Range (Units coordinateSystem) ->
    Range (Units coordinateSystem) ->
    Range (Units coordinateSystem) ->
    Bounds3d coordinateSystem

deriving instance Show (Bounds3d (space @ units))

instance Bounds.Interface (Bounds3d (space @ units)) where
  aggregate2 = aggregate2
  intersection = intersection

instance HasUnits (Bounds3d (space @ units)) where
  type Units (Bounds3d (space @ units)) = units
  type Erase (Bounds3d (space @ units)) = Bounds3d (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (Bounds3d (space @ units1)) (Bounds3d (space_ @ units2))
  where
  coerce = Data.Coerce.coerce

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Bounds3d (space @ units))
    (VectorBounds3d (space @ units))
    (Bounds3d (space_ @ units_))
  where
  Bounds3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = Bounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Bounds3d (space @ units))
    (VectorBounds3d (space @ units))
    (Bounds3d (space_ @ units_))
  where
  Bounds3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = Bounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point3d (space @ units))
    (Bounds3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  Point3d px py pz - Bounds3d bx by bz = VectorBounds3d (px - bx) (py - by) (pz - bz)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Bounds3d (space @ units))
    (Point3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  Bounds3d bx by bz - Point3d px py pz = VectorBounds3d (bx - px) (by - py) (bz - pz)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Bounds3d (space @ units))
    (Bounds3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  Bounds3d x1 y1 z1 - Bounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

xRange :: Bounds3d (space @ units) -> Range units
xRange (Bounds3d x _ _) = x

yRange :: Bounds3d (space @ units) -> Range units
yRange (Bounds3d _ y _) = y

zRange :: Bounds3d (space @ units) -> Range units
zRange (Bounds3d _ _ z) = z

{-# INLINE xyzRanges #-}
xyzRanges :: Bounds3d (space @ units) -> (Range units, Range units, Range units)
xyzRanges (Bounds3d x y z) = (x, y, z)

xyz :: Range units -> Range units -> Range units -> Bounds3d (space @ units)
xyz = Bounds3d

constant :: Point3d (space @ units) -> Bounds3d (space @ units)
constant (Point3d x y z) =
  Bounds3d (Range.constant x) (Range.constant y) (Range.constant z)

aggregate2 :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bounds3d (space @ units)
aggregate2 (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) =
  Bounds3d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2) (Range.aggregate2 z1 z2)

exclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Qty units
exclusion (Point3d x y z) (Bounds3d bx by bz) = do
  let dx = Range.exclusion x bx
  let dy = Range.exclusion y by
  let dz = Range.exclusion z bz
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  let pz = dz >= Qty.zero
  if
    | px && py && pz -> Qty.hypot3 dx dy dz
    | px && py -> Qty.hypot2 dx dy
    | px && pz -> Qty.hypot2 dx dz
    | py && pz -> Qty.hypot2 dy dz
    | px -> dx
    | py -> dy
    | pz -> dz
    | otherwise -> Qty.max (Qty.max dx dy) dz

inclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Qty units
inclusion point bounds = -(exclusion point bounds)

contains :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
contains (Bounds3d x2 y2 z2) (Bounds3d x1 y1 z1) =
  Range.contains x2 x1 && Range.contains y2 y1 && Range.contains z2 z1

isContainedIn :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Qty units
separation (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) = do
  let dx = Range.separation x1 x2
  let dy = Range.separation y1 y2
  let dz = Range.separation z1 z2
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  let pz = dz >= Qty.zero
  if
    | px && py && pz -> Qty.hypot3 dx dy dz
    | px && py -> Qty.hypot2 dx dy
    | px && pz -> Qty.hypot2 dx dz
    | py && pz -> Qty.hypot2 dy dz
    | px -> dx
    | py -> dy
    | pz -> dz
    | otherwise -> Qty.max (Qty.max dx dy) dz

overlap :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Qty units
overlap first second = -(separation first second)

intersection :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Maybe (Bounds3d (space @ units))
intersection (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) = Maybe.do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  z <- Range.intersection z1 z2
  Just (Bounds3d x y z)

hull2 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull2 (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Bounds3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull3 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) = do
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  let minZ = Qty.min (Qty.min z1 z2) z3
  let maxZ = Qty.max (Qty.max z1 z2) z3
  Bounds3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hull4 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull4 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) (Point3d x4 y4 z4) = do
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  let minZ = Qty.min (Qty.min (Qty.min z1 z2) z3) z4
  let maxZ = Qty.max (Qty.max (Qty.max z1 z2) z3) z4
  Bounds3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

interpolate :: Bounds3d (space @ units) -> Float -> Float -> Float -> Point3d (space @ units)
interpolate (Bounds3d x y z) u v w =
  Point3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)
