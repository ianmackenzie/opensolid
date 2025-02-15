module OpenSolid.VectorBounds3d
  ( VectorBounds3d (VectorBounds3d)
  , constant
  , xyz
  , aggregate2
  , aggregate3
  , hull2
  , hull3
  , hull4
  , hullN
  , xComponent
  , yComponent
  , zComponent
  , components
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , maxSquaredMagnitude'
  , normalize
  , includes
  , contains
  , isContainedIn
  , interpolate
  , relativeTo
  , placeIn
  , placeInBasis
  , relativeToBasis
  , transformBy
  , rotateIn
  , rotateAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Float qualified as Float
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d (Axis3d), Frame3d (Frame3d), VectorBounds3d (VectorBounds3d))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d

constant :: Vector3d (space @ units) -> VectorBounds3d (space @ units)
constant (Vector3d x y z) = VectorBounds3d (Range.constant x) (Range.constant y) (Range.constant z)

xyz :: Range units -> Range units -> Range units -> VectorBounds3d (space @ units)
xyz = VectorBounds3d

hull2 :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorBounds3d (space @ units)
hull2 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  VectorBounds3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull3 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) = do
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  let minZ = Qty.min (Qty.min z1 z2) z3
  let maxZ = Qty.max (Qty.max z1 z2) z3
  VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

hull4 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull4 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) = do
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  let minZ = Qty.min (Qty.min (Qty.min z1 z2) z3) z4
  let maxZ = Qty.max (Qty.max (Qty.max z1 z2) z3) z4
  VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

hullN :: NonEmpty (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
hullN (Vector3d x0 y0 z0 :| rest) = go x0 x0 y0 y0 z0 z0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> Qty units -> Qty units -> List (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
  go xLow xHigh yLow yHigh zLow zHigh [] = VectorBounds3d (Range xLow xHigh) (Range yLow yHigh) (Range zLow zHigh)
  go xLow xHigh yLow yHigh zLow zHigh (Vector3d x y z : remaining) =
    go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) (Qty.min zLow z) (Qty.max zHigh z) remaining

aggregate2 ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
aggregate2 (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) =
  VectorBounds3d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2) (Range.aggregate2 z1 z2)

aggregate3 ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
aggregate3 (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) (VectorBounds3d x3 y3 z3) =
  VectorBounds3d (Range.aggregate3 x1 x2 x3) (Range.aggregate3 y1 y2 y3) (Range.aggregate3 z1 z2 z3)

xComponent :: VectorBounds3d (space @ units) -> Range units
xComponent (VectorBounds3d vx _ _) = vx

yComponent :: VectorBounds3d (space @ units) -> Range units
yComponent (VectorBounds3d _ vy _) = vy

zComponent :: VectorBounds3d (space @ units) -> Range units
zComponent (VectorBounds3d _ _ vz) = vz

components :: VectorBounds3d (space @ units) -> (Range units, Range units, Range units)
components (VectorBounds3d vx vy vz) = (vx, vy, vz)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Range units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: VectorBounds3d (space @ units) -> Range (units :*: units)
squaredMagnitude' (VectorBounds3d x y z) = Range.squared' x + Range.squared' y + Range.squared' z

magnitude :: VectorBounds3d (space @ units) -> Range units
magnitude (VectorBounds3d x y z) = Range.hypot3 x y z

maxMagnitude :: VectorBounds3d (space @ units) -> Qty units
maxMagnitude (VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  let zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
  Qty.hypot3 xMagnitude yMagnitude zMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Qty units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude'

maxSquaredMagnitude' :: VectorBounds3d (space @ units) -> Qty (units :*: units)
maxSquaredMagnitude' (VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  let zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
  Qty.squared' xMagnitude + Qty.squared' yMagnitude + Qty.squared' zMagnitude

normalize :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ Unitless)
normalize vectorBounds = do
  let (VectorBounds3d x y z) = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  let nz = clampNormalized z
  VectorBounds3d nx ny nz

normalizedRange :: Range Unitless
normalizedRange = Range.from -1.0 1.0

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range (Qty.clampTo normalizedRange low) (Qty.clampTo normalizedRange high)

includes :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
includes (Vector3d vx vy vz) (VectorBounds3d x y z) =
  Range.includes vx x && Range.includes vy y && Range.includes vz z

contains :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
contains (VectorBounds3d x2 y2 z2) (VectorBounds3d x1 y1 z1) =
  Range.contains x2 x1 && Range.contains y2 y1 && Range.contains z2 z1

isContainedIn :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

interpolate :: VectorBounds3d (space @ units) -> Float -> Float -> Float -> Vector3d (space @ units)
interpolate (VectorBounds3d x y z) u v w =
  Vector3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  VectorBounds3d (local @ units) ->
  VectorBounds3d (global @ units)
placeIn (Frame3d _ basis) = placeInBasis basis

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  VectorBounds3d (global @ units) ->
  VectorBounds3d (local @ units)
relativeTo (Frame3d _ basis) = relativeToBasis basis

placeInBasis ::
  Basis3d global (Defines local) ->
  VectorBounds3d (local @ units) ->
  VectorBounds3d (global @ units)
placeInBasis basis (VectorBounds3d x y z) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let zMid = Range.midpoint z
  let xWidth = Range.width x
  let yWidth = Range.width y
  let zWidth = Range.width z
  let Vector3d x0 y0 z0 = Vector3d.xyzInBasis basis xMid yMid zMid
  let (ix, iy, iz) = Direction3d.components (Basis3d.xDirection basis)
  let (jx, jy, jz) = Direction3d.components (Basis3d.yDirection basis)
  let (kx, ky, kz) = Direction3d.components (Basis3d.zDirection basis)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx + 0.5 * zWidth * Float.abs kx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy + 0.5 * zWidth * Float.abs ky
  let rz = 0.5 * xWidth * Float.abs iz + 0.5 * yWidth * Float.abs jz + 0.5 * zWidth * Float.abs kz
  VectorBounds3d
    (Range.from (x0 - rx) (x0 + rx))
    (Range.from (y0 - ry) (y0 + ry))
    (Range.from (z0 - rz) (z0 + rz))

relativeToBasis ::
  Basis3d global (Defines local) ->
  VectorBounds3d (global @ units) ->
  VectorBounds3d (local @ units)
relativeToBasis basis (VectorBounds3d x y z) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let zMid = Range.midpoint z
  let xWidth = Range.width x
  let yWidth = Range.width y
  let zWidth = Range.width z
  let Vector3d x0 y0 z0 = Vector3d.relativeToBasis basis (Vector3d xMid yMid zMid)
  let (ix, iy, iz) = Direction3d.components (Basis3d.xDirection basis)
  let (jx, jy, jz) = Direction3d.components (Basis3d.yDirection basis)
  let (kx, ky, kz) = Direction3d.components (Basis3d.zDirection basis)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy + 0.5 * zWidth * Float.abs iz
  let ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy + 0.5 * zWidth * Float.abs jz
  let rz = 0.5 * xWidth * Float.abs kx + 0.5 * yWidth * Float.abs ky + 0.5 * zWidth * Float.abs kz
  VectorBounds3d
    (Range.from (x0 - rx) (x0 + rx))
    (Range.from (y0 - ry) (y0 + ry))
    (Range.from (z0 - rz) (z0 + rz))

transformBy ::
  Transform3d tag (space @ units1) ->
  VectorBounds3d (space @ units2) ->
  VectorBounds3d (space @ units2)
transformBy transform (VectorBounds3d x y z) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let zMid = Range.midpoint z
  let xWidth = Range.width x
  let yWidth = Range.width y
  let zWidth = Range.width z
  let Vector3d x0 y0 z0 = Vector3d.transformBy transform (Vector3d.xyz xMid yMid zMid)
  let (Transform3d _ i j k) = transform
  let (ix, iy, iz) = Vector3d.components i
  let (jx, jy, jz) = Vector3d.components j
  let (kx, ky, kz) = Vector3d.components k
  let rx = 0.5 * Float.abs ix * xWidth + 0.5 * Float.abs jx * yWidth + 0.5 * Float.abs kx * zWidth
  let ry = 0.5 * Float.abs iy * xWidth + 0.5 * Float.abs jy * yWidth + 0.5 * Float.abs ky * zWidth
  let rz = 0.5 * Float.abs iz * xWidth + 0.5 * Float.abs jz * yWidth + 0.5 * Float.abs kz * zWidth
  VectorBounds3d
    (Range.from (x0 - rx) (x0 + rx))
    (Range.from (y0 - ry) (y0 + ry))
    (Range.from (z0 - rz) (z0 + rz))

rotateIn ::
  Direction3d space ->
  Angle ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
rotateIn axisDirection = rotateAround (Axis3d Point3d.origin axisDirection)

rotateAround ::
  Axis3d (space @ axisUnits) ->
  Angle ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy
