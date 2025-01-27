module OpenSolid.Vector2d
  ( Vector2d (Vector2d#, Vector2d)
  , zero
  , unit
  , x
  , y
  , xy
  , xyIn
  , xyInBasis
  , fromComponents
  , from
  , meters
  , centimeters
  , millimeters
  , inches
  , squareMeters
  , polar
  , xComponent
  , yComponent
  , componentIn
  , projectionIn
  , components
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , squaredMagnitude'
  , angle
  , IsZero (IsZero)
  , direction
  , magnitudeAndDirection
  , normalize
  , rotateRight
  , rotateLeft
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , sum
  , transformBy
  , rotateBy
  , mirrorIn
  , mirrorAcross
  , scaleIn
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Arithmetic.Unboxed
import {-# SOURCE #-} OpenSolid.Axis2d (Axis2d)
import {-# SOURCE #-} OpenSolid.Axis2d qualified as Axis2d
import {-# SOURCE #-} OpenSolid.Basis2d (Basis2d)
import {-# SOURCE #-} OpenSolid.Basis2d qualified as Basis2d
import {-# SOURCE #-} OpenSolid.Direction2d (Direction2d)
import {-# SOURCE #-} OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Error qualified as Error
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import {-# SOURCE #-} OpenSolid.Frame2d (Frame2d)
import {-# SOURCE #-} OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import {-# SOURCE #-} OpenSolid.Point2d (Point2d)
import {-# SOURCE #-} OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty#))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (Meters, SquareMeters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d.CoordinateTransformation qualified as Vector2d.CoordinateTransformation
import {-# SOURCE #-} OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Vector2d phantom

-- | A vector in 2D, represented by its X and Y components.
data Vector2d (coordinateSystem :: CoordinateSystem) = Vector2d# Double# Double#

{-# COMPLETE Vector2d #-}

{-# INLINE Vector2d #-}
pattern Vector2d :: Qty units -> Qty units -> Vector2d (space @ units)
pattern Vector2d vx vy <- (components# -> (# vx, vy #))
  where
    Vector2d (Qty# vx#) (Qty# vy#) = Vector2d# vx# vy#

deriving instance Eq (Vector2d (space @ units))

deriving instance Show (Vector2d (space @ units))

instance FFI (Vector2d (space @ Unitless)) where
  representation = FFI.classRepresentation "Vector2d"

instance FFI (Vector2d (space @ Meters)) where
  representation = FFI.classRepresentation "Displacement2d"

instance FFI (Vector2d (space @ SquareMeters)) where
  representation = FFI.classRepresentation "AreaVector2d"

instance HasUnits (Vector2d (space @ units)) units (Vector2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector2d (space1 @ unitsA))
    (Vector2d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) units1
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector2d (space @ units)) where
  negate (Vector2d# vx# vy#) = Vector2d# (negate# vx#) (negate# vy#)

instance
  Multiplication'
    Sign
    (Vector2d (space @ units))
    (Vector2d (space @ (Unitless :*: units)))
  where
  Positive .*. vector = Units.coerce vector
  Negative .*. vector = Units.coerce -vector

instance Multiplication Sign (Vector2d (space @ units)) (Vector2d (space @ units)) where
  Positive * vector = vector
  Negative * vector = -vector

instance
  Multiplication'
    (Vector2d (space @ units))
    Sign
    (Vector2d (space @ (units :*: Unitless)))
  where
  vector .*. Positive = Units.coerce vector
  vector .*. Negative = Units.coerce -vector

instance Multiplication (Vector2d (space @ units)) Sign (Vector2d (space @ units)) where
  vector * Positive = vector
  vector * Negative = -vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Vector2d# x1# y1# + Vector2d# x2# y2# = Vector2d# (x1# +# x2#) (y1# +# y2#)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Vector2d# x1# y1# - Vector2d# x2# y2# = Vector2d# (x1# -# x2#) (y1# -# y2#)

instance
  Multiplication'
    (Qty units1)
    (Vector2d (space @ units2))
    (Vector2d (space @ (units1 :*: units2)))
  where
  Qty# scale# .*. Vector2d# vx# vy# = Vector2d# (scale# *# vx#) (scale# *# vy#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Vector2d (space @ units2)) (Vector2d (space @ units3))
  where
  Qty# scale# * Vector2d# vx# vy# = Vector2d# (scale# *# vx#) (scale# *# vy#)

instance
  Multiplication'
    (Vector2d (space @ units1))
    (Qty units2)
    (Vector2d (space @ (units1 :*: units2)))
  where
  Vector2d# vx# vy# .*. Qty# scale# = Vector2d# (vx# *# scale#) (vy# *# scale#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))
  where
  Vector2d# vx# vy# * Qty# scale# = Vector2d# (vx# *# scale#) (vy# *# scale#)

instance
  Multiplication'
    (Range units1)
    (Vector2d (space @ units2))
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  range .*. Vector2d vx vy = VectorBounds2d (range .*. vx) (range .*. vy)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (Vector2d (space @ units2)) (VectorBounds2d (space @ units3))
  where
  range * Vector2d vx vy = VectorBounds2d (range * vx) (range * vy)

instance
  Multiplication'
    (Vector2d (space @ units1))
    (Range units2)
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  Vector2d vx vy .*. range = VectorBounds2d (vx .*. range) (vy .*. range)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Range units2) (VectorBounds2d (space @ units3))
  where
  Vector2d vx vy * range = VectorBounds2d (vx * range) (vy * range)

instance
  Division'
    (Vector2d (space @ units1))
    (Qty units2)
    (Vector2d (space @ (units1 :/: units2)))
  where
  Vector2d# vx# vy# ./. Qty# scale# = Vector2d# (vx# /# scale#) (vy# /# scale#)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))
  where
  Vector2d# vx# vy# / Qty# scale# = Vector2d# (vx# /# scale#) (vy# /# scale#)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Qty (units1 :*: units2))
  where
  Vector2d# x1# y1# .<>. Vector2d# x2# y2# = Qty# (x1# *# x2# +# y1# *# y2#)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Qty units3)
  where
  Vector2d# x1# y1# <> Vector2d# x2# y2# = Qty# (x1# *# x2# +# y1# *# y2#)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector2d (space1 @ units)) (Direction2d space2) (Qty (units :*: Unitless))
  where
  v .<>. d = v .<>. unit d

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Qty units)
  where
  v <> d = v <> unit d

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (Vector2d (space2 @ units)) (Qty (Unitless :*: units))
  where
  d .<>. v = unit d .<>. v

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Qty units)
  where
  d <> v = unit d <> v

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Qty (units1 :*: units2))
  where
  Vector2d# x1# y1# .><. Vector2d# x2# y2# = Qty# (x1# *# y2# -# y1# *# x2#)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Qty units3)
  where
  Vector2d# x1# y1# >< Vector2d# x2# y2# = Qty# (x1# *# y2# -# y1# *# x2#)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector2d (space1 @ units)) (Direction2d space2) (Qty (units :*: Unitless))
  where
  v .><. d = v .><. unit d

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Qty units)
  where
  v >< d = v >< unit d

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (Vector2d (space2 @ units)) (Qty (Unitless :*: units))
  where
  d .><. v = unit d .><. v

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Qty units)
  where
  d >< v = unit d >< v

-- | The zero vector.
zero :: Vector2d (space @ units)
zero = Vector2d# 0.0## 0.0##

-- | Construct a unit vector in the given direction.
unit :: Direction2d space -> Vector2d (space @ Unitless)
unit = Direction2d.unwrap

{-| Construct a vector from just an X component.

The Y component will be set to zero.
-}
x :: forall space units. Qty units -> Vector2d (space @ units)
x (Qty# vx#) = Vector2d# vx# 0.0##

{-| Construct a vector from just a Y component.

The X component will be set to zero.
-}
y :: forall space units. Qty units -> Vector2d (space @ units)
y (Qty# vy#) = Vector2d# 0.0## vy#

-- | Construct a vector from its X and Y components.
xy :: forall space units. Qty units -> Qty units -> Vector2d (space @ units)
xy = Vector2d

xyIn :: Frame2d (space @ originUnits) defines -> Qty units -> Qty units -> Vector2d (space @ units)
xyIn frame = xyInBasis (Frame2d.basis frame)

xyInBasis :: Basis2d space defines -> Qty units -> Qty units -> Vector2d (space @ units)
xyInBasis basis vx vy = vx * Basis2d.xDirection basis + vy * Basis2d.yDirection basis

-- | Construct a vector from a pair of X and Y components.
fromComponents :: forall space units. (Qty units, Qty units) -> Vector2d (space @ units)
fromComponents (vx, vy) = Vector2d vx vy

from :: Point2d (space @ units) -> Point2d (space @ units) -> Vector2d (space @ units)
from p1 p2 = p2 - p1

apply :: (Float -> Qty units) -> Float -> Float -> Vector2d (space @ units)
apply units px py = Vector2d (units px) (units py)

-- | Construct a vector from its X and Y components given in meters.
meters :: Float -> Float -> Vector2d (space @ Meters)
meters = apply Length.meters

-- | Construct a vector from its X and Y components given in centimeters.
centimeters :: Float -> Float -> Vector2d (space @ Meters)
centimeters = apply Length.centimeters

-- | Construct a vector from its X and Y components given in millimeters.
millimeters :: Float -> Float -> Vector2d (space @ Meters)
millimeters = apply Length.millimeters

-- | Construct a vector from its X and Y components given in inches.
inches :: Float -> Float -> Vector2d (space @ Meters)
inches = apply Length.inches

-- | Construct a vector from its X and Y components given in square meters.
squareMeters :: Float -> Float -> Vector2d (space @ SquareMeters)
squareMeters = apply Area.squareMeters

-- | Construct a vector from its magnitude (length) and angle.
polar :: Qty units -> Angle -> Vector2d (space @ units)
polar (Qty# r#) (Qty# theta#) = Vector2d# (r# *# cos# theta#) (r# *# sin# theta#)

-- | Get the X component of a vector.
xComponent :: Vector2d (space @ units) -> Qty units
xComponent (Vector2d# vx# _) = Qty# vx#

-- | Get the Y component of a vector.
yComponent :: Vector2d (space @ units) -> Qty units
yComponent (Vector2d# _ vy#) = Qty# vy#

componentIn :: Direction2d space -> Vector2d (space @ units) -> Qty units
componentIn = (<>)

projectionIn :: Direction2d space -> Vector2d (space @ units) -> Vector2d (space @ units)
projectionIn givenDirection vector = givenDirection * componentIn givenDirection vector

-- | Get the X and Y components of a vector.
{-# INLINE components #-}
components :: Vector2d (space @ units) -> (Qty units, Qty units)
components (Vector2d# vx# vy#) = (Qty# vx#, Qty# vy#)

{-# INLINE components# #-}
components# :: Vector2d (space @ units) -> (# Qty units, Qty units #)
components# (Vector2d# vx# vy#) = (# Qty# vx#, Qty# vy# #)

interpolateFrom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Vector2d (space @ units)
interpolateFrom (Vector2d# x1# y1#) (Vector2d# x2# y2#) (Qty# t#) =
  Vector2d# (x1# +# t# *# (x2# -# x1#)) (y1# +# t# *# (y2# -# y1#))

midpoint :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units)
midpoint (Vector2d# x1# y1#) (Vector2d# x2# y2#) =
  Vector2d# (0.5## *# (x1# +# x2#)) (0.5## *# (y1# +# y2#))

magnitude :: Vector2d (space @ units) -> Qty units
magnitude (Vector2d# vx# vy#) = Qty# (sqrt# (vx# *# vx# +# vy# *# vy#))

squaredMagnitude :: Units.Squared units1 units2 => Vector2d (space @ units1) -> Qty units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: Vector2d (space @ units) -> Qty (units :*: units)
squaredMagnitude' (Vector2d# vx# vy#) = Qty# (vx# *# vx# +# vy# *# vy#)

{-| Get the angle of a vector.

The angle is measured counterclockwise from the positive X axis, so:

  * A vector in the positive X direction has an angle of zero.
  * A vector in the positive Y direction has an angle of 90 degrees.
  * A vector in the negative Y direction has an angle of -90 degrees.
  * It is not defined whether a vector exactly in the negative X direction has
    an angle of -180 or +180 degrees. (Currently it is reported as having an
    angle of +180 degrees, but this should not be relied upon.)

The returned angle will be between -180 and +180 degrees.
-}
angle :: Vector2d (space @ units) -> Angle
angle (Vector2d vx vy) = Angle.atan2 vy vx

data IsZero = IsZero deriving (Eq, Show)

instance Error.Message IsZero where
  message IsZero = "Vector is zero"

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector2d (space @ units) -> Result IsZero (Direction2d space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (Direction2d.unsafe (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector2d (space @ units) ->
  Result IsZero (Qty units, Direction2d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (vm, Direction2d.unsafe (vector / vm))

normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
normalize vector = do
  let vm = magnitude vector
  if vm == Qty.zero then zero else vector / vm

rotateLeft :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateLeft (Vector2d# vx# vy#) = Vector2d# (negate# vy#) vx#

rotateRight :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateRight (Vector2d# vx# vy#) = Vector2d# vy# (negate# vx#)

placeIn ::
  Frame2d (global @ originUnits) (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeIn = Vector2d.CoordinateTransformation.placeIn

relativeTo ::
  Frame2d (global @ originUnits) (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeTo = Vector2d.CoordinateTransformation.relativeTo

placeInBasis ::
  Basis2d global (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeInBasis = Vector2d.CoordinateTransformation.placeInBasis

relativeToBasis ::
  Basis2d global (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeToBasis = Vector2d.CoordinateTransformation.relativeToBasis

sum :: List (Vector2d (space @ units)) -> Vector2d (space @ units)
sum = List.foldl (+) zero

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units)
transformBy transform vector = do
  let Transform2d _ i j = transform
  let Vector2d vx vy = vector
  vx * i + vy * j

rotateBy :: Angle -> Vector2d (space @ units) -> Vector2d (space @ units)
rotateBy theta = transformBy (Transform2d.rotateAround Point2d.origin theta)

mirrorIn :: Direction2d space -> Vector2d (space @ units) -> Vector2d (space @ units)
mirrorIn mirrorDirection vector = vector - 2.0 * projectionIn mirrorDirection vector

mirrorAcross :: Axis2d (space @ originUnits) -> Vector2d (space @ units) -> Vector2d (space @ units)
mirrorAcross axis = mirrorIn (Axis2d.normalDirection axis)

scaleIn :: Direction2d space -> Float -> Vector2d (space @ units) -> Vector2d (space @ units)
scaleIn scaleDirection scale vector = vector + (scale - 1.0) * projectionIn scaleDirection vector
