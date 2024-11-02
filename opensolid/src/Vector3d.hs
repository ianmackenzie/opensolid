module Vector3d
  ( Vector3d (Vector3d)
  , zero
  , unit
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  , xyzIn
  , xyzInBasis
  , meters
  , centimeters
  , millimeters
  , inches
  , squareMeters
  , xComponent
  , yComponent
  , zComponent
  , componentIn
  , projectionIn
  , components
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , squaredMagnitude'
  , IsZero (IsZero)
  , direction
  , magnitudeAndDirection
  , normalize
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , sum
  , transformBy
  , rotateIn
  , scaleIn
  , scaleAbout
  )
where

import Area qualified
import Arithmetic.Unboxed
import {-# SOURCE #-} Axis3d (Axis3d)
import {-# SOURCE #-} Axis3d qualified
import {-# SOURCE #-} Basis3d (Basis3d)
import {-# SOURCE #-} Basis3d qualified
import Data.Coerce qualified
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} Direction3d qualified
import Error qualified
import Float qualified
import {-# SOURCE #-} Frame3d (Frame3d)
import {-# SOURCE #-} Frame3d qualified
import Length qualified
import List qualified
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)
import {-# SOURCE #-} Point3d qualified
import Qty (Qty (Qty#))
import Qty qualified
import Transform3d (Transform3d (Transform3d))
import Transform3d qualified
import Units (Meters, SquareMeters)
import Units qualified
import Vector3d.CoordinateTransformation qualified

type role Vector3d phantom

data Vector3d (coordinateSystem :: CoordinateSystem) = Vector3d# Double# Double# Double#

{-# COMPLETE Vector3d #-}

{-# INLINE Vector3d #-}
pattern Vector3d :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)
pattern Vector3d vx vy vz <- (components# -> (# vx, vy, vz #))
  where
    Vector3d (Qty# vx#) (Qty# vy#) (Qty# vz#) = Vector3d# vx# vy# vz#

deriving instance Eq (Vector3d (space @ units))

deriving instance Show (Vector3d (space @ units))

instance HasUnits (Vector3d (space @ units)) where
  type UnitsOf (Vector3d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector3d (space1 @ unitsA))
    (Vector3d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  ApproximateEquality (Vector3d (space @ units)) (Vector3d (space_ @ units_)) units
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector3d (space @ units)) where
  negate (Vector3d# vx# vy# vz#) = Vector3d# (negate# vx#) (negate# vy#) (negate# vz#)

instance Multiplication' Sign (Vector3d (space @ units)) where
  type Sign .*. Vector3d (space @ units) = Vector3d (space @ (Unitless :*: units))
  Positive .*. vector = Units.coerce vector
  Negative .*. vector = Units.coerce -vector

instance Multiplication Sign (Vector3d (space @ units)) (Vector3d (space @ units))

instance Multiplication' (Vector3d (space @ units)) Sign where
  type Vector3d (space @ units) .*. Sign = Vector3d (space @ (units :*: Unitless))
  vector .*. Positive = Units.coerce vector
  vector .*. Negative = Units.coerce -vector

instance Multiplication (Vector3d (space @ units)) Sign (Vector3d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Vector3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Vector3d# x1# y1# z1# + Vector3d# x2# y2# z2# = Vector3d# (x1# +# x2#) (y1# +# y2#) (z1# +# z2#)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Vector3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Vector3d# x1# y1# z1# - Vector3d# x2# y2# z2# = Vector3d# (x1# -# x2#) (y1# -# y2#) (z1# -# z2#)

instance Multiplication' Int (Vector3d (space @ units)) where
  type Int .*. Vector3d (space @ units) = Vector3d (space @ (Unitless :*: units))
  scale .*. vector = Float.int scale .*. vector

instance Multiplication Int (Vector3d (space @ units)) (Vector3d (space @ units))

instance Multiplication' (Vector3d (space @ units)) Int where
  type Vector3d (space @ units) .*. Int = Vector3d (space @ (units :*: Unitless))
  vector .*. scale = vector .*. Float.int scale

instance Multiplication (Vector3d (space @ units)) Int (Vector3d (space @ units))

instance Multiplication' (Qty units1) (Vector3d (space @ units2)) where
  type Qty units1 .*. Vector3d (space @ units2) = Vector3d (space @ (units1 :*: units2))
  Qty# scale# .*. Vector3d# vx# vy# vz# = Vector3d# (scale# *# vx#) (scale# *# vy#) (scale# *# vz#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Vector3d (space @ units2)) (Vector3d (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Qty units2) where
  type Vector3d (space @ units1) .*. Qty units2 = Vector3d (space @ (units1 :*: units2))
  Vector3d# vx# vy# vz# .*. Qty# scale# = Vector3d# (vx# *# scale#) (vy# *# scale#) (vz# *# scale#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))

instance Division' (Vector3d (space @ units1)) (Qty units2) where
  type Vector3d (space @ units1) ./. Qty units2 = Vector3d (space @ (units1 :/: units2))
  Vector3d# vx# vy# vz# ./. Qty# scale# = Vector3d# (vx# /# scale#) (vy# /# scale#) (vz# /# scale#)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))

instance Division' (Vector3d (space @ units)) Int where
  type Vector3d (space @ units) ./. Int = Vector3d (space @ (units :/: Unitless))
  vector ./. scale = vector ./. Float.int scale

instance Division (Vector3d (space @ units)) Int (Vector3d (space @ units))

instance
  space ~ space_ =>
  DotMultiplication' (Vector3d (space @ units1)) (Vector3d (space_ @ units2))
  where
  type Vector3d (space @ units1) .<>. Vector3d (space_ @ units2) = Qty (units1 :*: units2)
  Vector3d# x1# y1# z1# .<>. Vector3d# x2# y2# z2# = Qty# (x1# *# x2# +# y1# *# y2# +# z1# *# z2#)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (Vector3d (space @ units1)) (Vector3d (space_ @ units2)) (Qty units3)

instance
  space ~ space_ =>
  DotMultiplication' (Vector3d (space @ units)) (Direction3d space_)
  where
  type Vector3d (space @ units) .<>. Direction3d space_ = Qty (units :*: Unitless)
  v .<>. d = v .<>. unit d

instance
  space ~ space_ =>
  DotMultiplication (Vector3d (space @ units)) (Direction3d space_) (Qty units)

instance
  space ~ space_ =>
  DotMultiplication' (Direction3d space) (Vector3d (space_ @ units))
  where
  type Direction3d space .<>. Vector3d (space_ @ units) = Qty (Unitless :*: units)
  d .<>. v = unit d .<>. v

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (Vector3d (space_ @ units)) (Qty units)

instance
  space ~ space_ =>
  CrossMultiplication' (Vector3d (space @ units1)) (Vector3d (space_ @ units2))
  where
  type Vector3d (space @ units1) .><. Vector3d (space_ @ units2) = Vector3d (space @ (units1 :*: units2))
  Vector3d# x1# y1# z1# .><. Vector3d# x2# y2# z2# =
    Vector3d#
      (y1# *# z2# -# z1# *# y2#)
      (z1# *# x2# -# x1# *# z2#)
      (x1# *# y2# -# y1# *# x2#)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication (Vector3d (space @ units1)) (Vector3d (space_ @ units2)) (Vector3d (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Vector3d (space @ units)) (Direction3d space_)
  where
  type Vector3d (space @ units) .><. Direction3d space_ = Vector3d (space @ (units :*: Unitless))
  v .><. d = v .><. unit d

instance
  space ~ space_ =>
  CrossMultiplication (Vector3d (space @ units)) (Direction3d space_) (Vector3d (space @ units))

instance
  space ~ space_ =>
  CrossMultiplication' (Direction3d space) (Vector3d (space_ @ units))
  where
  type Direction3d space .><. Vector3d (space_ @ units) = Vector3d (space @ (Unitless :*: units))
  d .><. v = unit d .><. v

instance
  space ~ space_ =>
  CrossMultiplication (Direction3d space) (Vector3d (space_ @ units)) (Vector3d (space @ units))

zero :: Vector3d (space @ units)
zero = Vector3d# 0.0## 0.0## 0.0##

unit :: Direction3d space -> Vector3d (space @ Unitless)
unit = Direction3d.unwrap

x :: Qty units -> Vector3d (space @ units)
x (Qty# vx#) = Vector3d# vx# 0.0## 0.0##

y :: Qty units -> Vector3d (space @ units)
y (Qty# vy#) = Vector3d# 0.0## vy# 0.0##

z :: Qty units -> Vector3d (space @ units)
z (Qty# vz#) = Vector3d# 0.0## 0.0## vz#

xy :: Qty units -> Qty units -> Vector3d (space @ units)
xy (Qty# vx#) (Qty# vz#) = Vector3d# vx# vz# 0.0##

xz :: Qty units -> Qty units -> Vector3d (space @ units)
xz (Qty# vx#) (Qty# vz#) = Vector3d# vx# 0.0## vz#

yz :: Qty units -> Qty units -> Vector3d (space @ units)
yz (Qty# vy#) (Qty# vz#) = Vector3d# 0.0## vy# vz#

xyz :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)
xyz = Vector3d

xyzIn ::
  Frame3d (space @ originUnits) defines ->
  Qty units ->
  Qty units ->
  Qty units ->
  Vector3d (space @ units)
xyzIn frame = xyzInBasis (Frame3d.basis frame)

xyzInBasis ::
  Basis3d space defines ->
  Qty units ->
  Qty units ->
  Qty units ->
  Vector3d (space @ units)
xyzInBasis basis vx vy vz =
  vx * Basis3d.xDirection basis + vy * Basis3d.yDirection basis + vz * Basis3d.zDirection basis

apply :: (Float -> Qty units) -> Float -> Float -> Float -> Vector3d (space @ units)
apply units px py pz = Vector3d (units px) (units py) (units pz)

meters :: Float -> Float -> Float -> Vector3d (space @ Meters)
meters = apply Length.meters

centimeters :: Float -> Float -> Float -> Vector3d (space @ Meters)
centimeters = apply Length.centimeters

millimeters :: Float -> Float -> Float -> Vector3d (space @ Meters)
millimeters = apply Length.millimeters

inches :: Float -> Float -> Float -> Vector3d (space @ Meters)
inches = apply Length.inches

squareMeters :: Float -> Float -> Float -> Vector3d (space @ SquareMeters)
squareMeters vx vy vz =
  Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

xComponent :: Vector3d (space @ units) -> Qty units
xComponent (Vector3d# vx# _ _) = Qty# vx#

yComponent :: Vector3d (space @ units) -> Qty units
yComponent (Vector3d# _ vy# _) = Qty# vy#

zComponent :: Vector3d (space @ units) -> Qty units
zComponent (Vector3d# _ _ vz#) = Qty# vz#

componentIn :: Direction3d space -> Vector3d (space @ units) -> Qty units
componentIn = (<>)

projectionIn :: Direction3d space -> Vector3d (space @ units) -> Vector3d (space @ units)
projectionIn givenDirection vector = givenDirection * componentIn givenDirection vector

{-# INLINE components #-}
components :: Vector3d (space @ units) -> (Qty units, Qty units, Qty units)
components (Vector3d vx vy vz) = (vx, vy, vz)

{-# INLINE components# #-}
components# :: Vector3d (space @ units) -> (# Qty units, Qty units, Qty units #)
components# (Vector3d# vx# vy# vz#) = (# Qty# vx#, Qty# vy#, Qty# vz# #)

interpolateFrom ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Float ->
  Vector3d (space @ units)
interpolateFrom (Vector3d# x1# y1# z1#) (Vector3d# x2# y2# z2#) (Qty# t#) =
  Vector3d#
    (x1# +# t# *# (x2# -# x1#))
    (y1# +# t# *# (y2# -# y1#))
    (z1# +# t# *# (z2# -# z1#))

midpoint :: Vector3d (space @ units) -> Vector3d (space @ units) -> Vector3d (space @ units)
midpoint (Vector3d# x1# y1# z1#) (Vector3d# x2# y2# z2#) =
  Vector3d# (0.5## *# (x1# +# x2#)) (0.5## *# (y1# +# y2#)) (0.5## *# (z1# +# z2#))

magnitude :: Vector3d (space @ units) -> Qty units
magnitude (Vector3d# vx# vy# vz#) = Qty# (sqrt# (vx# *# vx# +# vy# *# vy# +# vz# *# vz#))

squaredMagnitude :: Units.Squared units1 units2 => Vector3d (space @ units1) -> Qty units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: Vector3d (space @ units) -> Qty (units :*: units)
squaredMagnitude' (Vector3d# vx# vy# vz#) = Qty# (vx# *# vx# +# vy# *# vy# +# vz# *# vz#)

data IsZero = IsZero deriving (Eq, Show, Error.Message)

direction :: Tolerance units => Vector3d (space @ units) -> Result IsZero (Direction3d space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (Direction3d.unsafe (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Result IsZero (Qty units, Direction3d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (vm, Direction3d.unsafe (vector / vm))

normalize :: Vector3d (space @ units) -> Vector3d (space @ Unitless)
normalize vector = do
  let vm = magnitude vector
  if vm == Qty.zero then zero else vector / vm

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
placeIn = Vector3d.CoordinateTransformation.placeIn

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
relativeTo = Vector3d.CoordinateTransformation.relativeTo

placeInBasis ::
  Basis3d global (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
placeInBasis = Vector3d.CoordinateTransformation.placeInBasis

relativeToBasis ::
  Basis3d global (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
relativeToBasis = Vector3d.CoordinateTransformation.relativeToBasis

sum :: List (Vector3d (space @ units)) -> Vector3d (space @ units)
sum = List.foldl (+) zero

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
transformBy transform vector = do
  let Transform3d _ i j k = transform
  let Vector3d vx vy vz = vector
  vx * i + vy * j + vz * k

rotateIn :: Direction3d space -> Angle -> Vector3d (space @ units) -> Vector3d (space @ units)
rotateIn axisDirection = rotateAround (Axis3d.through Point3d.origin axisDirection)

scaleIn :: Direction3d space -> Float -> Vector3d (space @ units) -> Vector3d (space @ units)
scaleIn axisDirection = scaleAlong (Axis3d.through Point3d.origin axisDirection)

rotateAround ::
  Axis3d (space @ axisUnits) ->
  Angle ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy

scaleAbout ::
  Point3d (space @ pointUnits) ->
  Float ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
scaleAbout = Transform3d.scaleAboutImpl transformBy

scaleAlong ::
  Axis3d (space @ axisUnits) ->
  Float ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
scaleAlong = Transform3d.scaleAlongImpl transformBy
