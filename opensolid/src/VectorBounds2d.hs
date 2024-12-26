-- Needed for 'Range * Vector2d = VectorBounds2d'
-- and 'Vector2d * Range = VectorBounds2d' instances
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorBounds2d
  ( VectorBounds2d (VectorBounds2d)
  , constant
  , xy
  , aggregate2
  , aggregate3
  , hull2
  , hull3
  , hull4
  , hullN
  , polar
  , xComponent
  , yComponent
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

import Basis2d (Basis2d)
import Basis2d qualified
import Data.Coerce qualified
import Direction2d (Direction2d)
import Direction2d qualified
import Float qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import Range (Range (Range))
import Range qualified
import Transform2d (Transform2d (Transform2d))
import Units (Radians)
import Units qualified

type role VectorBounds2d phantom

data VectorBounds2d (coordinateSystem :: CoordinateSystem) where
  VectorBounds2d ::
    Range (UnitsOf coordinateSystem) ->
    Range (UnitsOf coordinateSystem) ->
    VectorBounds2d coordinateSystem

deriving instance Show (VectorBounds2d (space @ units))

instance HasUnits (VectorBounds2d (space @ units)) where
  type UnitsOf (VectorBounds2d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds2d (space1 @ unitsA)) (VectorBounds2d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Vector2d (space1 @ units1)) (VectorBounds2d (space2 @ units2)) units1
  where
  Vector2d vx vy ^ VectorBounds2d bx by = vx ^ bx && vy ^ by

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (VectorBounds2d (space1 @ units1)) (Vector2d (space2 @ units2)) units1
  where
  bounds ^ point = point ^ bounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (VectorBounds2d (space1 @ units1)) (VectorBounds2d (space2 @ units2)) units1
  where
  VectorBounds2d x1 y1 ^ VectorBounds2d x2 y2 = x1 ^ x2 && y1 ^ y2

instance Negation (VectorBounds2d (space @ units)) where
  negate (VectorBounds2d x y) = VectorBounds2d (negate x) (negate y)

instance Multiplication' Sign (VectorBounds2d (space @ units)) where
  type Sign .*. VectorBounds2d (space @ units) = VectorBounds2d (space @ (Unitless :*: units))
  Positive .*. vectorBounds = Units.coerce vectorBounds
  Negative .*. vectorBounds = Units.coerce -vectorBounds

instance Multiplication Sign (VectorBounds2d (space @ units)) (VectorBounds2d (space @ units))

instance Multiplication' (VectorBounds2d (space @ units)) Sign where
  type VectorBounds2d (space @ units) .*. Sign = VectorBounds2d (space @ (units :*: Unitless))
  vectorBounds .*. Positive = Units.coerce vectorBounds
  vectorBounds .*. Negative = Units.coerce -vectorBounds

instance Multiplication (VectorBounds2d (space @ units)) Sign (VectorBounds2d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorBounds2d (space @ units))
    (VectorBounds2d (space_ @ units_))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 + VectorBounds2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorBounds2d (space @ units))
    (Vector2d (space_ @ units_))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 + Vector2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector2d (space @ units))
    (VectorBounds2d (space_ @ units_))
    (VectorBounds2d (space @ units))
  where
  Vector2d x1 y1 + VectorBounds2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorBounds2d (space @ units))
    (VectorBounds2d (space_ @ units_))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 - VectorBounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorBounds2d (space @ units))
    (Vector2d (space_ @ units_))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 - Vector2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector2d (space @ units))
    (VectorBounds2d (space_ @ units_))
    (VectorBounds2d (space @ units))
  where
  Vector2d x1 y1 - VectorBounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance Multiplication' (Qty units1) (VectorBounds2d (space @ units2)) where
  type Qty units1 .*. VectorBounds2d (space @ units2) = VectorBounds2d (space @ (units1 :*: units2))
  value .*. VectorBounds2d x y = VectorBounds2d (value .*. x) (value .*. y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorBounds2d (space @ units2)) (VectorBounds2d (space @ units3))

instance Multiplication' (VectorBounds2d (space @ units1)) (Qty units2) where
  type VectorBounds2d (space @ units1) .*. Qty units2 = VectorBounds2d (space @ (units1 :*: units2))
  VectorBounds2d x y .*. value = VectorBounds2d (x .*. value) (y .*. value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds2d (space @ units1)) (Qty units2) (VectorBounds2d (space @ units3))

instance Multiplication' (Range units1) (Vector2d (space @ units2)) where
  type Range units1 .*. Vector2d (space @ units2) = VectorBounds2d (space @ (units1 :*: units2))
  range .*. Vector2d x y = VectorBounds2d (range .*. x) (range .*. y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (Vector2d (space @ units2)) (VectorBounds2d (space @ units3))

instance Multiplication' (Vector2d (space @ units1)) (Range units2) where
  type Vector2d (space @ units1) .*. Range units2 = VectorBounds2d (space @ (units1 :*: units2))
  Vector2d x y .*. range = VectorBounds2d (x .*. range) (y .*. range)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Range units2) (VectorBounds2d (space @ units3))

instance Multiplication' (Range units1) (VectorBounds2d (space @ units2)) where
  type Range units1 .*. VectorBounds2d (space @ units2) = VectorBounds2d (space @ (units1 :*: units2))
  range .*. VectorBounds2d x y = VectorBounds2d (range .*. x) (range .*. y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (VectorBounds2d (space @ units2)) (VectorBounds2d (space @ units3))

instance Multiplication' (VectorBounds2d (space @ units1)) (Range units2) where
  type VectorBounds2d (space @ units1) .*. Range units2 = VectorBounds2d (space @ (units1 :*: units2))
  VectorBounds2d x y .*. range = VectorBounds2d (x .*. range) (y .*. range)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds2d (space @ units1)) (Range units2) (VectorBounds2d (space @ units3))

instance Division' (VectorBounds2d (space @ units1)) (Qty units2) where
  type VectorBounds2d (space @ units1) ./. Qty units2 = VectorBounds2d (space @ (units1 :/: units2))
  VectorBounds2d x y ./. value = VectorBounds2d (x ./. value) (y ./. value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2d (space @ units1)) (Qty units2) (VectorBounds2d (space @ units3))

instance Division' (VectorBounds2d (space @ units1)) (Range units2) where
  type VectorBounds2d (space @ units1) ./. Range units2 = VectorBounds2d (space @ (units1 :/: units2))
  VectorBounds2d x y ./. range = VectorBounds2d (x ./. range) (y ./. range)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2d (space @ units1)) (Range units2) (VectorBounds2d (space @ units3))

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (Vector2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  DotMultiplication' (Vector2d (space @ units1)) (VectorBounds2d (space_ @ units2))
  where
  type Vector2d (space @ units1) .<>. VectorBounds2d (space_ @ units2) = Range (units1 :*: units2)
  Vector2d x1 y1 .<>. VectorBounds2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (VectorBounds2d (space @ units1)) (Vector2d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds2d (space @ units1)) (Vector2d (space_ @ units2))
  where
  type VectorBounds2d (space @ units1) .<>. Vector2d (space_ @ units2) = Range (units1 :*: units2)
  VectorBounds2d x1 y1 .<>. Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  space ~ space_ =>
  DotMultiplication (Direction2d space) (VectorBounds2d (space_ @ units)) (Range units)

instance
  space ~ space_ =>
  DotMultiplication' (Direction2d space) (VectorBounds2d (space_ @ units))
  where
  type Direction2d space .<>. VectorBounds2d (space_ @ units) = Range (Unitless :*: units)
  direction .<>. vectorBounds = Vector2d.unit direction .<>. vectorBounds

instance
  space ~ space_ =>
  DotMultiplication (VectorBounds2d (space @ units)) (Direction2d space_) (Range units)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds2d (space @ units)) (Direction2d space_)
  where
  type VectorBounds2d (space @ units) .<>. Direction2d space_ = Range (units :*: Unitless)
  vectorBounds .<>. direction = vectorBounds .<>. Vector2d.unit direction

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (VectorBounds2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds2d (space @ units1)) (VectorBounds2d (space_ @ units2))
  where
  type VectorBounds2d (space @ units1) .<>. VectorBounds2d (space_ @ units2) = Range (units1 :*: units2)
  VectorBounds2d x1 y1 .<>. VectorBounds2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication (Vector2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  CrossMultiplication' (Vector2d (space @ units1)) (VectorBounds2d (space_ @ units2))
  where
  type Vector2d (space @ units1) .><. VectorBounds2d (space_ @ units2) = Range (units1 :*: units2)
  Vector2d x1 y1 .><. VectorBounds2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication (VectorBounds2d (space @ units1)) (Vector2d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  CrossMultiplication' (VectorBounds2d (space @ units1)) (Vector2d (space_ @ units2))
  where
  type VectorBounds2d (space @ units1) .><. Vector2d (space_ @ units2) = Range (units1 :*: units2)
  VectorBounds2d x1 y1 .><. Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance space ~ space_ => CrossMultiplication (Direction2d space) (VectorBounds2d (space_ @ units)) (Range units)

instance
  space ~ space_ =>
  CrossMultiplication' (Direction2d space) (VectorBounds2d (space_ @ units))
  where
  type Direction2d space .><. VectorBounds2d (space_ @ units) = Range (Unitless :*: units)
  direction .><. vectorBounds = Vector2d.unit direction .><. vectorBounds

instance space ~ space_ => CrossMultiplication (VectorBounds2d (space @ units)) (Direction2d space_) (Range units)

instance
  space ~ space_ =>
  CrossMultiplication' (VectorBounds2d (space @ units)) (Direction2d space_)
  where
  type VectorBounds2d (space @ units) .><. Direction2d space_ = Range (units :*: Unitless)
  vectorBounds .><. direction = vectorBounds .><. Vector2d.unit direction

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication (VectorBounds2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)

instance
  space ~ space_ =>
  CrossMultiplication' (VectorBounds2d (space @ units1)) (VectorBounds2d (space_ @ units2))
  where
  type VectorBounds2d (space @ units1) .><. VectorBounds2d (space_ @ units2) = Range (units1 :*: units2)
  VectorBounds2d x1 y1 .><. VectorBounds2d x2 y2 = x1 .*. y2 - y1 .*. x2

constant :: Vector2d (space @ units) -> VectorBounds2d (space @ units)
constant (Vector2d x y) = VectorBounds2d (Range.constant x) (Range.constant y)

xy :: Range units -> Range units -> VectorBounds2d (space @ units)
xy = VectorBounds2d

hull2 :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorBounds2d (space @ units)
hull2 (Vector2d x1 y1) (Vector2d x2 y2) =
  VectorBounds2d (Range.from x1 x2) (Range.from y1 y2)

hull3 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBounds2d (space @ units)
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) = do
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  VectorBounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBounds2d (space @ units)
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) = do
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  VectorBounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hullN :: NonEmpty (Vector2d (space @ units)) -> VectorBounds2d (space @ units)
hullN (Vector2d x0 y0 :| rest) = go x0 x0 y0 y0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> List (Vector2d (space @ units)) -> VectorBounds2d (space @ units)
  go xLow xHigh yLow yHigh [] = VectorBounds2d (Range.unsafe xLow xHigh) (Range.unsafe yLow yHigh)
  go xLow xHigh yLow yHigh (Vector2d x y : remaining) =
    go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) remaining

aggregate2 ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units)
aggregate2 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) =
  VectorBounds2d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2)

aggregate3 ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units)
aggregate3 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) (VectorBounds2d x3 y3) =
  VectorBounds2d (Range.aggregate3 x1 x2 x3) (Range.aggregate3 y1 y2 y3)

polar :: Range units -> Range Radians -> VectorBounds2d (space @ units)
polar r theta = VectorBounds2d (r * Range.cos theta) (r * Range.sin theta)

xComponent :: VectorBounds2d (space @ units) -> Range units
xComponent (VectorBounds2d vx _) = vx

yComponent :: VectorBounds2d (space @ units) -> Range units
yComponent (VectorBounds2d _ vy) = vy

components :: VectorBounds2d (space @ units) -> (Range units, Range units)
components (VectorBounds2d vx vy) = (vx, vy)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d (space @ units1) -> Range units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: VectorBounds2d (space @ units) -> Range (units :*: units)
squaredMagnitude' (VectorBounds2d x y) = Range.squared' x + Range.squared' y

magnitude :: VectorBounds2d (space @ units) -> Range units
magnitude (VectorBounds2d x y) = Range.hypot2 x y

maxMagnitude :: VectorBounds2d (space @ units) -> Qty units
maxMagnitude (VectorBounds2d (Range minX maxX) (Range minY maxY)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  Qty.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d (space @ units1) -> Qty units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude'

maxSquaredMagnitude' :: VectorBounds2d (space @ units) -> Qty (units :*: units)
maxSquaredMagnitude' (VectorBounds2d (Range minX maxX) (Range minY maxY)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  Qty.squared' xMagnitude + Qty.squared' yMagnitude

normalize :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ Unitless)
normalize vectorBounds = do
  let (VectorBounds2d x y) = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  VectorBounds2d nx ny

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

includes :: Vector2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
includes (Vector2d vx vy) (VectorBounds2d x y) = Range.includes vx x && Range.includes vy y

contains :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
contains (VectorBounds2d x2 y2) (VectorBounds2d x1 y1) =
  Range.contains x2 x1 && Range.contains y2 y1

isContainedIn :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

interpolate :: VectorBounds2d (space @ units) -> Float -> Float -> Vector2d (space @ units)
interpolate (VectorBounds2d x y) u v =
  Vector2d.xy (Range.interpolate x u) (Range.interpolate y v)

placeIn ::
  Frame2d (global @ originUnits) (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds2d (global @ units)
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo ::
  Frame2d (global @ originUnits) (Defines local) ->
  VectorBounds2d (global @ units) ->
  VectorBounds2d (local @ units)
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis ::
  Basis2d global (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds2d (global @ units)
placeInBasis basis (VectorBounds2d x y) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let Vector2d x0 y0 = Vector2d.xyInBasis basis xMid yMid
  let (ix, iy) = Direction2d.components (Basis2d.xDirection basis)
  let (jx, jy) = Direction2d.components (Basis2d.yDirection basis)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
  VectorBounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

relativeToBasis ::
  Basis2d global (Defines local) ->
  VectorBounds2d (global @ units) ->
  VectorBounds2d (local @ units)
relativeToBasis basis (VectorBounds2d x y) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let Vector2d x0 y0 = Vector2d.relativeToBasis basis (Vector2d xMid yMid)
  let (ix, iy) = Direction2d.components (Basis2d.xDirection basis)
  let (jx, jy) = Direction2d.components (Basis2d.yDirection basis)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy
  let ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy
  VectorBounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

transformBy ::
  Transform2d tag (space @ units1) ->
  VectorBounds2d (space @ units2) ->
  VectorBounds2d (space @ units2)
transformBy transform (VectorBounds2d x y) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let (x0, y0) = Vector2d.components (Vector2d.transformBy transform (Vector2d.xy xMid yMid))
  let (Transform2d _ i j) = transform
  let (ix, iy) = Vector2d.components i
  let (jx, jy) = Vector2d.components j
  let rx = 0.5 * Float.abs ix * xWidth + 0.5 * Float.abs jx * yWidth
  let ry = 0.5 * Float.abs iy * xWidth + 0.5 * Float.abs jy * yWidth
  VectorBounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))
