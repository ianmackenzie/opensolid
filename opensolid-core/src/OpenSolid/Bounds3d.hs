module OpenSolid.Bounds3d
  ( Bounds3d (Bounds3d)
  , Bounded3d (bounds)
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , coordinates
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
  , diameter
  , interpolate
  , transformBy
  )
where

import OpenSolid.Float qualified as Float
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3d (Bounds3d))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Vector3d (Vector3d (Vector3d))

class Bounded3d a (coordinateSystem :: CoordinateSystem) | a -> coordinateSystem where
  bounds :: a -> Bounds3d coordinateSystem

instance Bounded3d (Point3d (space @ units)) (space @ units) where
  bounds = constant

instance Bounded3d (Bounds3d (space @ units)) (space @ units) where
  bounds = identity

xCoordinate :: Bounds3d (space @ units) -> Range units
xCoordinate (Bounds3d x _ _) = x

yCoordinate :: Bounds3d (space @ units) -> Range units
yCoordinate (Bounds3d _ y _) = y

zCoordinate :: Bounds3d (space @ units) -> Range units
zCoordinate (Bounds3d _ _ z) = z

{-# INLINE coordinates #-}
coordinates :: Bounds3d (space @ units) -> (Range units, Range units, Range units)
coordinates (Bounds3d x y z) = (x, y, z)

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
inclusion point box = -(exclusion point box)

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
  Bounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

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
  Bounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

diameter :: Bounds3d (space @ units) -> Qty units
diameter (Bounds3d x y z) = Qty.hypot3 (Range.width x) (Range.width y) (Range.width z)

interpolate :: Bounds3d (space @ units) -> Float -> Float -> Float -> Point3d (space @ units)
interpolate (Bounds3d x y z) u v w =
  Point3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)

transformBy ::
  Transform3d tag (space @ units) ->
  Bounds3d (space @ units) ->
  Bounds3d (space @ units)
transformBy transform (Bounds3d x y z) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let zMid = Range.midpoint z
  let xWidth = Range.width x
  let yWidth = Range.width y
  let zWidth = Range.width z
  let Point3d x0 y0 z0 = Point3d.transformBy transform (Point3d xMid yMid zMid)
  let Transform3d _ i j k = transform
  let Vector3d ix iy iz = i
  let Vector3d jx jy jz = j
  let Vector3d kx ky kz = k
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx + 0.5 * zWidth * Float.abs kx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy + 0.5 * zWidth * Float.abs ky
  let rz = 0.5 * xWidth * Float.abs iz + 0.5 * yWidth * Float.abs jz + 0.5 * zWidth * Float.abs kz
  Bounds3d (Range (x0 - rx) (x0 + rx)) (Range (y0 - ry) (y0 + ry)) (Range (z0 - rz) (z0 + rz))
