-- Needed for 'Range * Vector3d = VectorBounds3d'
-- and 'Vector3d * Range = VectorBounds3d' instances
{-# OPTIONS_GHC -Wno-orphans #-}

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
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Float qualified as Float
import {-# SOURCE #-} OpenSolid.Frame3d (Frame3d)
import {-# SOURCE #-} OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d

type role VectorBounds3d nominal

data VectorBounds3d (coordinateSystem :: CoordinateSystem) where
  VectorBounds3d ::
    Range units ->
    Range units ->
    Range units ->
    VectorBounds3d (space @ units)

deriving instance Show (VectorBounds3d (space @ units))

instance HasUnits (VectorBounds3d (space @ units)) where
  type UnitsOf (VectorBounds3d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds3d (space1 @ unitsA)) (VectorBounds3d (space2 @ unitsB))
  where
  coerce (VectorBounds3d x y z) = VectorBounds3d (Units.coerce x) (Units.coerce y) (Units.coerce z)

instance Negation (VectorBounds3d (space @ units)) where
  negate (VectorBounds3d x y z) = VectorBounds3d (negate x) (negate y) (negate z)

instance Multiplication' Sign (VectorBounds3d (space @ units)) where
  type Sign .*. VectorBounds3d (space @ units) = VectorBounds3d (space @ (Unitless :*: units))
  Positive .*. vectorBounds = Units.coerce vectorBounds
  Negative .*. vectorBounds = Units.coerce -vectorBounds

instance Multiplication Sign (VectorBounds3d (space @ units)) (VectorBounds3d (space @ units))

instance Multiplication' (VectorBounds3d (space @ units)) Sign where
  type VectorBounds3d (space @ units) .*. Sign = VectorBounds3d (space @ (units :*: Unitless))
  vectorBounds .*. Positive = Units.coerce vectorBounds
  vectorBounds .*. Negative = Units.coerce -vectorBounds

instance Multiplication (VectorBounds3d (space @ units)) Sign (VectorBounds3d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorBounds3d (space @ units))
    (VectorBounds3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorBounds3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 + Vector3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (VectorBounds3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  Vector3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorBounds3d (space @ units))
    (VectorBounds3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorBounds3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 - Vector3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (VectorBounds3d (space_ @ units_))
    (VectorBounds3d (space @ units))
  where
  Vector3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication' (Qty units1) (VectorBounds3d (space @ units2)) where
  type
    Qty units1 .*. VectorBounds3d (space @ units2) =
      VectorBounds3d (space @ (units1 :*: units2))
  value .*. VectorBounds3d x y z = VectorBounds3d (value .*. x) (value .*. y) (value .*. z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorBounds3d (space @ units2)) (VectorBounds3d (space @ units3))

instance Multiplication' (VectorBounds3d (space @ units1)) (Qty units2) where
  type
    VectorBounds3d (space @ units1) .*. Qty units2 =
      VectorBounds3d (space @ (units1 :*: units2))
  VectorBounds3d x y z .*. value = VectorBounds3d (x .*. value) (y .*. value) (z .*. value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds3d (space @ units1)) (Qty units2) (VectorBounds3d (space @ units3))

instance Multiplication' (Range units1) (Vector3d (space @ units2)) where
  type Range units1 .*. Vector3d (space @ units2) = VectorBounds3d (space @ (units1 :*: units2))
  range .*. Vector3d x y z = VectorBounds3d (range .*. x) (range .*. y) (range .*. z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (Vector3d (space @ units2)) (VectorBounds3d (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Range units2) where
  type Vector3d (space @ units1) .*. Range units2 = VectorBounds3d (space @ (units1 :*: units2))
  Vector3d x y z .*. range = VectorBounds3d (x .*. range) (y .*. range) (z .*. range)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Range units2) (VectorBounds3d (space @ units3))

instance Multiplication' (Range units1) (VectorBounds3d (space @ units2)) where
  type
    Range units1 .*. VectorBounds3d (space @ units2) =
      VectorBounds3d (space @ (units1 :*: units2))
  range .*. VectorBounds3d x y z = VectorBounds3d (range .*. x) (range .*. y) (range .*. z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (VectorBounds3d (space @ units2)) (VectorBounds3d (space @ units3))

instance Multiplication' (VectorBounds3d (space @ units1)) (Range units2) where
  type
    VectorBounds3d (space @ units1) .*. Range units2 =
      VectorBounds3d (space @ (units1 :*: units2))
  VectorBounds3d x y z .*. range = VectorBounds3d (x .*. range) (y .*. range) (z .*. range)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds3d (space @ units1)) (Range units2) (VectorBounds3d (space @ units3))

instance Division' (VectorBounds3d (space @ units1)) (Qty units2) where
  type
    VectorBounds3d (space @ units1) ./. Qty units2 =
      VectorBounds3d (space @ (units1 :/: units2))
  VectorBounds3d x y z ./. value = VectorBounds3d (x ./. value) (y ./. value) (z ./. value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3d (space @ units1)) (Qty units2) (VectorBounds3d (space @ units3))

instance Division' (VectorBounds3d (space @ units1)) (Range units2) where
  type
    VectorBounds3d (space @ units1) ./. Range units2 =
      VectorBounds3d (space @ (units1 :/: units2))
  VectorBounds3d x y z ./. range = VectorBounds3d (x ./. range) (y ./. range) (z ./. range)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3d (space @ units1)) (Range units2) (VectorBounds3d (space @ units3))

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (Vector3d (space @ units1)) (VectorBounds3d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  DotMultiplication' (Vector3d (space @ units1)) (VectorBounds3d (space_ @ units2))
  where
  type Vector3d (space @ units1) .<>. VectorBounds3d (space_ @ units2) = Range (units1 :*: units2)
  Vector3d x1 y1 z1 .<>. VectorBounds3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (VectorBounds3d (space @ units1)) (Vector3d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds3d (space @ units1)) (Vector3d (space_ @ units2))
  where
  type VectorBounds3d (space @ units1) .<>. Vector3d (space_ @ units2) = Range (units1 :*: units2)
  VectorBounds3d x1 y1 z1 .<>. Vector3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (VectorBounds3d (space_ @ units)) (Range units)

instance
  space ~ space_ =>
  DotMultiplication' (Direction3d space) (VectorBounds3d (space_ @ units))
  where
  type Direction3d space .<>. VectorBounds3d (space_ @ units) = Range (Unitless :*: units)
  direction .<>. vectorBounds = Vector3d.unit direction .<>. vectorBounds

instance
  space ~ space_ =>
  DotMultiplication (VectorBounds3d (space @ units)) (Direction3d space_) (Range units)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds3d (space @ units)) (Direction3d space_)
  where
  type VectorBounds3d (space @ units) .<>. Direction3d space_ = Range (units :*: Unitless)
  vectorBounds .<>. direction = vectorBounds .<>. Vector3d.unit direction

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (VectorBounds3d (space @ units1)) (VectorBounds3d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds3d (space @ units1)) (VectorBounds3d (space_ @ units2))
  where
  type
    VectorBounds3d (space @ units1) .<>. VectorBounds3d (space_ @ units2) =
      Range (units1 :*: units2)
  VectorBounds3d x1 y1 z1 .<>. VectorBounds3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (VectorBounds3d (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Vector3d (space @ units1)) (VectorBounds3d (space_ @ units2))
  where
  type
    Vector3d (space @ units1) .><. VectorBounds3d (space_ @ units2) =
      VectorBounds3d (space @ (units1 :*: units2))
  Vector3d x1 y1 z1 .><. VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 .*. z2 - z1 .*. y2)
      (z1 .*. x2 - x1 .*. z2)
      (x1 .*. y2 - y1 .*. x2)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorBounds3d (space @ units1))
    (Vector3d (space_ @ units2))
    (VectorBounds3d (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (VectorBounds3d (space @ units1)) (Vector3d (space_ @ units2))
  where
  type
    VectorBounds3d (space @ units1) .><. Vector3d (space_ @ units2) =
      VectorBounds3d (space @ (units1 :*: units2))
  VectorBounds3d x1 y1 z1 .><. Vector3d x2 y2 z2 =
    VectorBounds3d
      (y1 .*. z2 - z1 .*. y2)
      (z1 .*. x2 - x1 .*. z2)
      (x1 .*. y2 - y1 .*. x2)

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorBounds3d (space2 @ units))
    (VectorBounds3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction3d space1) (VectorBounds3d (space2 @ units))
  where
  type
    Direction3d space1 .><. VectorBounds3d (space2 @ units) =
      VectorBounds3d (space1 @ (Unitless :*: units))
  direction .><. vectorBounds = Vector3d.unit direction .><. vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorBounds3d (space1 @ units))
    (Direction3d space2)
    (VectorBounds3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorBounds3d (space1 @ units)) (Direction3d space2)
  where
  type
    VectorBounds3d (space1 @ units) .><. Direction3d space2 =
      VectorBounds3d (space1 @ (units :*: Unitless))
  vectorBounds .><. direction = vectorBounds .><. Vector3d.unit direction

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (VectorBounds3d (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (VectorBounds3d (space @ units1)) (VectorBounds3d (space_ @ units2))
  where
  type
    VectorBounds3d (space @ units1) .><. VectorBounds3d (space_ @ units2) =
      VectorBounds3d (space @ (units1 :*: units2))
  VectorBounds3d x1 y1 z1 .><. VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 .*. z2 - z1 .*. y2)
      (z1 .*. x2 - x1 .*. z2)
      (x1 .*. y2 - y1 .*. x2)

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
  VectorBounds3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

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
  VectorBounds3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hullN :: NonEmpty (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
hullN (Vector3d x0 y0 z0 :| rest) = go x0 x0 y0 y0 z0 z0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> Qty units -> Qty units -> List (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
  go xLow xHigh yLow yHigh zLow zHigh [] = VectorBounds3d (Range.unsafe xLow xHigh) (Range.unsafe yLow yHigh) (Range.unsafe zLow zHigh)
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

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

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
placeIn frame = placeInBasis (Frame3d.basis frame)

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  VectorBounds3d (global @ units) ->
  VectorBounds3d (local @ units)
relativeTo frame = relativeToBasis (Frame3d.basis frame)

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
  let rz = 0.5 * xWidth * Float.abs iz + 0.5 * zWidth * Float.abs jz + 0.5 * zWidth * Float.abs kz
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
