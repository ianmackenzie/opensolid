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

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (VectorBounds3d (space @ units1'))
    (VectorBounds3d (space' @ units2'))
  where
  coerce = Data.Coerce.coerce

instance Negation (VectorBounds3d (space @ units)) where
  negate (VectorBounds3d x y z) = VectorBounds3d (negate x) (negate y) (negate z)

instance Multiplication Sign (VectorBounds3d (space @ units)) (VectorBounds3d (space @ units)) where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = -vectorBounds

instance Multiplication (VectorBounds3d (space @ units)) Sign (VectorBounds3d (space @ units)) where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = -vectorBounds

instance
  (units ~ units', space ~ space') =>
  Addition
    (VectorBounds3d (space @ units))
    (VectorBounds3d (space' @ units'))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (units ~ units', space ~ space') =>
  Addition
    (VectorBounds3d (space @ units))
    (Vector3d (space' @ units'))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 + Vector3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (units ~ units', space ~ space') =>
  Addition
    (Vector3d (space @ units))
    (VectorBounds3d (space' @ units'))
    (VectorBounds3d (space @ units))
  where
  Vector3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (VectorBounds3d (space @ units))
    (VectorBounds3d (space' @ units'))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (VectorBounds3d (space @ units))
    (Vector3d (space' @ units'))
    (VectorBounds3d (space @ units))
  where
  VectorBounds3d x1 y1 z1 - Vector3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Vector3d (space @ units))
    (VectorBounds3d (space' @ units'))
    (VectorBounds3d (space @ units))
  where
  Vector3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ units3))
  where
  value * VectorBounds3d x y z = VectorBounds3d (value * x) (value * y) (value * z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds3d (space @ units1))
    (Qty units2)
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z * value = VectorBounds3d (x * value) (y * value) (z * value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Range units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ units3))
  where
  range * VectorBounds3d x y z = VectorBounds3d (range * x) (range * y) (range * z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds3d (space @ units1))
    (Range units2)
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z * range = VectorBounds3d (x * range) (y * range) (z * range)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorBounds3d (space @ units1))
    (Qty units2)
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z / value = VectorBounds3d (x / value) (y / value) (z / value)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorBounds3d (space @ units1))
    (Range units2)
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z / range = VectorBounds3d (x / range) (y / range) (z / range)

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (Vector3d (space @ units1))
    (VectorBounds3d (space' @ units2))
    (Range units3)
  where
  Vector3d x1 y1 z1 <> VectorBounds3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (VectorBounds3d (space @ units1))
    (Vector3d (space' @ units2))
    (Range units3)
  where
  VectorBounds3d x1 y1 z1 <> Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space ~ space' =>
  DotProduct
    (Direction3d space)
    (VectorBounds3d (space' @ units))
    (Range units)
  where
  Direction3d vector <> vectorBounds = vector <> vectorBounds

instance
  space ~ space' =>
  DotProduct
    (VectorBounds3d (space @ units))
    (Direction3d space')
    (Range units)
  where
  vectorBounds <> Direction3d vector = vectorBounds <> vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space' @ units2))
    (Range units3)
  where
  VectorBounds3d x1 y1 z1 <> VectorBounds3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (Vector3d (space @ units1))
    (VectorBounds3d (space' @ units2))
    (VectorBounds3d (space @ units3))
  where
  Vector3d x1 y1 z1 >< VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (VectorBounds3d (space @ units1))
    (Vector3d (space' @ units2))
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x1 y1 z1 >< Vector3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space' @ units2))
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x1 y1 z1 >< VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

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
hull3 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) =
  let minX = Qty.min (Qty.min x1 x2) x3
      maxX = Qty.max (Qty.max x1 x2) x3
      minY = Qty.min (Qty.min y1 y2) y3
      maxY = Qty.max (Qty.max y1 y2) y3
      minZ = Qty.min (Qty.min z1 z2) z3
      maxZ = Qty.max (Qty.max z1 z2) z3
   in VectorBounds3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

hull4 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull4 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) =
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
      maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
      minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
      maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
      minZ = Qty.min (Qty.min (Qty.min z1 z2) z3) z4
      maxZ = Qty.max (Qty.max (Qty.max z1 z2) z3) z4
   in VectorBounds3d (Range.unsafe minX maxX) (Range.unsafe minY maxY) (Range.unsafe minZ maxZ)

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
maxMagnitude (VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)) =
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
      yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
      zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
   in Qty.hypot3 xMagnitude yMagnitude zMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Qty units2
maxSquaredMagnitude (VectorBounds3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)) =
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
      yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
      zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
   in Qty.squared xMagnitude + Qty.squared yMagnitude + Qty.squared zMagnitude

normalize :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ Unitless)
normalize vectorBounds =
  let (VectorBounds3d x y z) = vectorBounds / magnitude vectorBounds
      nx = clampNormalized x
      ny = clampNormalized y
      nz = clampNormalized z
   in VectorBounds3d nx ny nz

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

interpolate :: VectorBounds3d (space @ units) -> Float -> Float -> Float -> Vector3d (space @ units)
interpolate (VectorBounds3d x y z) u v w =
  Vector3d (Range.interpolate x u) (Range.interpolate y v) (Range.interpolate z w)
