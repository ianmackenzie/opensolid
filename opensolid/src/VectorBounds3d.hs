-- Needed for 'Range * Vector3d = VectorBounds3d'
-- and 'Vector3d * Range = VectorBounds3d' instances
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorBounds3d
  ( VectorBounds3d (VectorBounds3d)
  , constant
  , xyz
  , hull2
  , hull3
  , hull4
  , xComponent
  , yComponent
  , zComponent
  , squaredMagnitude
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , normalize
  , interpolate
  )
where

import Data.Coerce qualified
import Direction3d (Direction3d (Direction3d))
import Float qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified
import Vector3d (Vector3d (Vector3d))

data VectorBounds3d (coordinateSystem :: CoordinateSystem) where
  VectorBounds3d ::
    Range (Units coordinateSystem) ->
    Range (Units coordinateSystem) ->
    Range (Units coordinateSystem) ->
    VectorBounds3d coordinateSystem

deriving instance Show (VectorBounds3d (space @ units))

instance HasUnits (VectorBounds3d (space @ units)) where
  type Units (VectorBounds3d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds3d (space1 @ unitsA)) (VectorBounds3d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

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

instance Multiplication' Int (VectorBounds3d (space @ units)) where
  type Int .*. VectorBounds3d (space @ units) = VectorBounds3d (space @ (Unitless :*: units))
  value .*. bounds = Float.int value .*. bounds

instance Multiplication Int (VectorBounds3d (space @ units)) (VectorBounds3d (space @ units))

instance Multiplication' (VectorBounds3d (space @ units)) Int where
  type VectorBounds3d (space @ units) .*. Int = VectorBounds3d (space @ (units :*: Unitless))
  vectorBounds .*. value = vectorBounds .*. Float.int value

instance Multiplication (VectorBounds3d (space @ units)) Int (VectorBounds3d (space @ units))

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

instance Division' (VectorBounds3d (space @ units)) Int where
  type VectorBounds3d (space @ units) ./. Int = VectorBounds3d (space @ (units :/: Unitless))
  vectorBounds ./. value = vectorBounds ./. Float.int value

instance Division (VectorBounds3d (space @ units)) Int (VectorBounds3d (space @ units))

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
  Direction3d vector .<>. vectorBounds = vector .<>. vectorBounds

instance
  space ~ space_ =>
  DotMultiplication (VectorBounds3d (space @ units)) (Direction3d space_) (Range units)

instance
  space ~ space_ =>
  DotMultiplication' (VectorBounds3d (space @ units)) (Direction3d space_)
  where
  type VectorBounds3d (space @ units) .<>. Direction3d space_ = Range (units :*: Unitless)
  vectorBounds .<>. Direction3d vector = vectorBounds .<>. vector

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

xComponent :: VectorBounds3d (space @ units) -> Range units
xComponent (VectorBounds3d vx _ _) = vx

yComponent :: VectorBounds3d (space @ units) -> Range units
yComponent (VectorBounds3d _ vy _) = vy

zComponent :: VectorBounds3d (space @ units) -> Range units
zComponent (VectorBounds3d _ _ vz) = vz

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Range units2
squaredMagnitude (VectorBounds3d x y z) = Range.squared x + Range.squared y + Range.squared z

magnitude :: VectorBounds3d (space @ units) -> Range units
magnitude (VectorBounds3d x y z) = Range.hypot3 x y z

maxMagnitude :: VectorBounds3d (space @ units) -> Qty units
maxMagnitude (VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  let zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
  Qty.hypot3 xMagnitude yMagnitude zMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Qty units2
maxSquaredMagnitude (VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  let zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
  Qty.squared xMagnitude + Qty.squared yMagnitude + Qty.squared zMagnitude

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

interpolate :: VectorBounds3d (space @ units) -> Float -> Float -> Float -> Vector3d (space @ units)
interpolate (VectorBounds3d x y z) u v w =
  Vector3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)
