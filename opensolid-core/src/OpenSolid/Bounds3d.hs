module OpenSolid.Bounds3d
  ( Bounds3d (Bounds3d)
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , coordinates
  , constant
  , hull2
  , hull3
  , hull4
  , hullN
  , aggregate2
  , aggregateN
  , exclusion
  , inclusion
  , contains
  , isContainedIn
  , separation
  , overlap
  , intersection
  , diameter
  , interpolate
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Direction3d (Direction3d (Direction3d))
import OpenSolid.Float qualified as Float
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Basis3d (Basis3d), Bounds3d (Bounds3d), Frame3d (Frame3d))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Vector3d (Vector3d (Vector3d))

-- | Get the X coordinate bounds of a bounding box.
xCoordinate :: Bounds3d (space @ units) -> Bounds units
xCoordinate (Bounds3d x _ _) = x

-- | Get the Y coordinate bounds of a bounding box.
yCoordinate :: Bounds3d (space @ units) -> Bounds units
yCoordinate (Bounds3d _ y _) = y

-- | Get the Z coordinate bounds of a bounding box.
zCoordinate :: Bounds3d (space @ units) -> Bounds units
zCoordinate (Bounds3d _ _ z) = z

-- | Get the XYZ coordinate bounds of a bounding box as a tuple.
{-# INLINE coordinates #-}
coordinates :: Bounds3d (space @ units) -> (Bounds units, Bounds units, Bounds units)
coordinates (Bounds3d x y z) = (x, y, z)

-- | Construct a zero-size bounding box containing a single point.
constant :: Point3d (space @ units) -> Bounds3d (space @ units)
constant (Point3d x y z) =
  Bounds3d (Bounds.constant x) (Bounds.constant y) (Bounds.constant z)

aggregate2 :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bounds3d (space @ units)
aggregate2 (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) =
  Bounds3d (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2) (Bounds.aggregate2 z1 z2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds3d (space @ units)) -> Bounds3d (space @ units)
aggregateN (Bounds3d (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) (Bounds zLow0 zHigh0) :| rest) =
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 zLow0 zHigh0 rest

aggregateImpl ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  List (Bounds3d (space @ units)) ->
  Bounds3d (space @ units)
aggregateImpl xLow xHigh yLow yHigh zLow zHigh rest = case rest of
  [] -> Bounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  next : remaining -> do
    let Bounds3d xNext yNext zNext = next
    let Bounds xLowNext xHighNext = xNext
    let Bounds yLowNext yHighNext = yNext
    let Bounds zLowNext zHighNext = zNext
    aggregateImpl
      (Qty.min xLow xLowNext)
      (Qty.max xHigh xHighNext)
      (Qty.min yLow yLowNext)
      (Qty.max yHigh yHighNext)
      (Qty.min zLow zLowNext)
      (Qty.max zHigh zHighNext)
      remaining

exclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Qty units
exclusion (Point3d x y z) (Bounds3d bx by bz) = do
  let dx = Bounds.exclusion x bx
  let dy = Bounds.exclusion y by
  let dz = Bounds.exclusion z bz
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
  Bounds.contains x2 x1 && Bounds.contains y2 y1 && Bounds.contains z2 z1

isContainedIn :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Qty units
separation (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) = do
  let dx = Bounds.separation x1 x2
  let dy = Bounds.separation y1 y2
  let dz = Bounds.separation z1 z2
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
  x <- Bounds.intersection x1 x2
  y <- Bounds.intersection y1 y2
  z <- Bounds.intersection z1 z2
  Just (Bounds3d x y z)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull2 (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Bounds3d (Bounds x1 x2) (Bounds y1 y2) (Bounds z1 z2)

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
  Bounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

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
  Bounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

-- | Construct a bounding box containing all points in the given non-empty list.
hullN :: NonEmpty (Point3d (space @ units)) -> Bounds3d (space @ units)
hullN (Point3d x0 y0 z0 :| rest) = accumulateHull x0 x0 y0 y0 z0 z0 rest

accumulateHull ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  List (Point3d (space @ units)) ->
  Bounds3d (space @ units)
accumulateHull xLow xHigh yLow yHigh zLow zHigh remaining = case remaining of
  [] -> Bounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  Point3d x y z : following ->
    accumulateHull
      (Qty.min xLow x)
      (Qty.max xHigh x)
      (Qty.min yLow y)
      (Qty.max yHigh y)
      (Qty.min zLow z)
      (Qty.max zHigh z)
      following

diameter :: Bounds3d (space @ units) -> Qty units
diameter (Bounds3d x y z) = Qty.hypot3 (Bounds.width x) (Bounds.width y) (Bounds.width z)

interpolate :: Bounds3d (space @ units) -> Float -> Float -> Float -> Point3d (space @ units)
interpolate (Bounds3d x y z) u v w =
  Point3d (Bounds.interpolate x u) (Bounds.interpolate y v) (Bounds.interpolate z w)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Bounds3d (local @ units) ->
  Bounds3d (global @ units)
placeIn frame (Bounds3d x y z) = do
  let Frame3d _ (Basis3d i j k) = frame
  let Direction3d ix iy iz = i
  let Direction3d jx jy jz = j
  let Direction3d kx ky kz = k
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let zMid = Bounds.midpoint z
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let zWidth = Bounds.width z
  let Point3d x0 y0 z0 = Point3d.placeIn frame (Point3d xMid yMid zMid)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx + 0.5 * zWidth * Float.abs kx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy + 0.5 * zWidth * Float.abs ky
  let rz = 0.5 * xWidth * Float.abs iz + 0.5 * yWidth * Float.abs jz + 0.5 * zWidth * Float.abs kz
  Bounds3d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry)) (Bounds (z0 - rz) (z0 + rz))

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Bounds3d (global @ units) ->
  Bounds3d (local @ units)
relativeTo frame (Bounds3d x y z) = do
  let Frame3d _ (Basis3d i j k) = frame
  let Direction3d ix iy iz = i
  let Direction3d jx jy jz = j
  let Direction3d kx ky kz = k
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let zMid = Bounds.midpoint z
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let zWidth = Bounds.width z
  let Point3d x0 y0 z0 = Point3d.relativeTo frame (Point3d xMid yMid zMid)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy + 0.5 * zWidth * Float.abs iz
  let ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy + 0.5 * zWidth * Float.abs jz
  let rz = 0.5 * xWidth * Float.abs kx + 0.5 * yWidth * Float.abs ky + 0.5 * zWidth * Float.abs kz
  Bounds3d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry)) (Bounds (z0 - rz) (z0 + rz))

transformBy ::
  Transform3d tag (space @ units) ->
  Bounds3d (space @ units) ->
  Bounds3d (space @ units)
transformBy transform (Bounds3d x y z) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let zMid = Bounds.midpoint z
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let zWidth = Bounds.width z
  let Point3d x0 y0 z0 = Point3d.transformBy transform (Point3d xMid yMid zMid)
  let Transform3d _ i j k = transform
  let Vector3d ix iy iz = i
  let Vector3d jx jy jz = j
  let Vector3d kx ky kz = k
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx + 0.5 * zWidth * Float.abs kx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy + 0.5 * zWidth * Float.abs ky
  let rz = 0.5 * xWidth * Float.abs iz + 0.5 * yWidth * Float.abs jz + 0.5 * zWidth * Float.abs kz
  Bounds3d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry)) (Bounds (z0 - rz) (z0 + rz))
