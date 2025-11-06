{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Primitives
  ( Vector2d (Vector2d, Vector2d##)
  , Direction2d (Unit2d, Direction2d)
  , Orientation2d (Orientation2d)
  , Point2d (Point2d, Position2d)
  , VectorBounds2d (VectorBounds2d)
  , Bounds2d (Bounds2d, PositionBounds2d)
  , Axis2d (Axis2d)
  , Frame2d (Frame2d)
  , Transform2d (Transform2d)
  , Vector3d (Vector3d, Vector3d##)
  , Direction3d (Unit3d, Direction3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Orientation3d (Orientation3d)
  , Point3d (Point3d, Position3d)
  , VectorBounds3d (VectorBounds3d, VectorBounds3d##)
  , Bounds3d (Bounds3d, PositionBounds3d)
  , Axis3d (Axis3d)
  , Plane3d (Plane3d)
  , Frame3d (Frame3d)
  , Transform3d (Transform3d)
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds (Bounds (Bounds##))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.HasZero (HasZero)
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance ((~=##))
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units

----- Vector2d -----

data Vector2d (coordinateSystem :: CoordinateSystem) = Vector2d## Double# Double#
  deriving (Eq, Ord, Show)

-- | Construct a vector from its X and Y components.
{-# INLINE Vector2d #-}
pattern Vector2d :: Quantity units -> Quantity units -> Vector2d (space @ units)
pattern Vector2d vx vy <- (viewVector2d -> (# vx, vy #))
  where
    Vector2d (Quantity## vx##) (Quantity## vy##) = Vector2d## vx## vy##

{-# INLINE viewVector2d #-}
viewVector2d :: Vector2d (space @ units) -> (# Quantity units, Quantity units #)
viewVector2d (Vector2d## vx## vy##) = (# Quantity## vx##, Quantity## vy## #)

{-# COMPLETE Vector2d #-}

instance HasField "xComponent" (Vector2d (space @ units)) (Quantity units) where
  getField (Vector2d vx _) = vx

instance HasField "yComponent" (Vector2d (space @ units)) (Quantity units) where
  getField (Vector2d _ vy) = vy

instance HasField "components" (Vector2d (space @ units)) (Quantity units, Quantity units) where
  getField (Vector2d vx vy) = (vx, vy)

instance HasField "angle" (Vector2d (space @ units)) Angle where
  getField (Vector2d vx vy) = Angle.atan2 vy vx

instance FFI (Vector2d (FFI.Space @ Unitless)) where
  representation = FFI.classRepresentation "Vector2d"

instance FFI (Vector2d (FFI.Space @ Meters)) where
  representation = FFI.classRepresentation "Displacement2d"

instance FFI (Vector2d (FFI.Space @ SquareMeters)) where
  representation = FFI.classRepresentation "AreaVector2d"

instance FFI (Vector2d UvCoordinates) where
  representation = FFI.classRepresentation "UvVector"

instance HasUnits (Vector2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector2d (space1 @ unitsA))
    (Vector2d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance ApproximateEquality (Vector2d (space @ units)) units where
  Vector2d## x1## y1## ~= Vector2d## x2## y2## =
    case hypot2## (x2## -## x1##) (y2## -## y1##) ~=## 0.0## of 1# -> True; _ -> False

instance HasZero (Vector2d (space @ units)) where
  zero = Vector2d## 0.0## 0.0##

instance Negation (Vector2d (space @ units)) where
  negative (Vector2d## vx## vy##) = Vector2d## (negate## vx##) (negate## vy##)

instance Multiplication Sign (Vector2d (space @ units)) (Vector2d (space @ units)) where
  Positive * vector = vector
  Negative * vector = negative vector

instance Multiplication (Vector2d (space @ units)) Sign (Vector2d (space @ units)) where
  vector * Positive = vector
  vector * Negative = negative vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Vector2d## x1## y1## + Vector2d## x2## y2## = Vector2d## (x1## +## x2##) (y1## +## y2##)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Vector2d## x1## y1## - Vector2d## x2## y2## = Vector2d## (x1## -## x2##) (y1## -## y2##)

instance
  Multiplication#
    (Quantity units1)
    (Vector2d (space @ units2))
    (Vector2d (space @ (units1 #*# units2)))
  where
  Quantity## scale## #*# Vector2d## vx## vy## = Vector2d## (scale## *## vx##) (scale## *## vy##)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Vector2d (space @ units2)) (Vector2d (space @ units3))
  where
  Quantity## scale## * Vector2d## vx## vy## = Vector2d## (scale## *## vx##) (scale## *## vy##)

instance
  Multiplication#
    (Vector2d (space @ units1))
    (Quantity units2)
    (Vector2d (space @ (units1 #*# units2)))
  where
  Vector2d## vx## vy## #*# Quantity## scale## = Vector2d## (vx## *## scale##) (vy## *## scale##)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Quantity units2) (Vector2d (space @ units3))
  where
  Vector2d## vx## vy## * Quantity## scale## = Vector2d## (vx## *## scale##) (vy## *## scale##)

instance
  Multiplication#
    (Bounds units1)
    (Vector2d (space @ units2))
    (VectorBounds2d (space @ (units1 #*# units2)))
  where
  bounds #*# Vector2d vx vy = VectorBounds2d (bounds #*# vx) (bounds #*# vy)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Vector2d (space @ units2)) (VectorBounds2d (space @ units3))
  where
  bounds * Vector2d vx vy = VectorBounds2d (bounds * vx) (bounds * vy)

instance
  Multiplication#
    (Vector2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ (units1 #*# units2)))
  where
  Vector2d vx vy #*# bounds = VectorBounds2d (vx #*# bounds) (vy #*# bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Bounds units2) (VectorBounds2d (space @ units3))
  where
  Vector2d vx vy * bounds = VectorBounds2d (vx * bounds) (vy * bounds)

instance
  Division#
    (Vector2d (space @ units1))
    (Quantity units2)
    (Vector2d (space @ (units1 #/# units2)))
  where
  Vector2d vx vy #/# scale = Vector2d (vx #/# scale) (vy #/# scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector2d (space @ units1)) (Quantity units2) (Vector2d (space @ units3))
  where
  Vector2d vx vy / scale = Vector2d (vx / scale) (vy / scale)

instance
  space1 ~ space2 =>
  DotMultiplication#
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Quantity (units1 #*# units2))
  where
  Vector2d x1 y1 `dot#` Vector2d x2 y2 = x1 #*# x2 + y1 #*# y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Quantity units3)
  where
  Vector2d x1 y1 `dot` Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Quantity units)
  where
  v `dot` Unit2d d = v `dot` d

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Quantity units)
  where
  Unit2d d `dot` v = d `dot` v

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Quantity (units1 #*# units2))
  where
  Vector2d x1 y1 `cross#` Vector2d x2 y2 = x1 #*# y2 - y1 #*# x2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Quantity units3)
  where
  Vector2d x1 y1 `cross` Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Quantity units)
  where
  v1 `cross` Unit2d v2 = v1 `cross` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Quantity units)
  where
  Unit2d v1 `cross` v2 = v1 `cross` v2

----- Direction2d -----

{-| A direction in 2D.

This is effectively a type-safe unit vector.
-}
newtype Direction2d (space :: Type) = Unit2d (Vector2d (space @ Unitless))
  deriving (Eq, Ord, Show)

{-# COMPLETE Direction2d #-}

{-# INLINE Direction2d #-}
pattern Direction2d :: Number -> Number -> Direction2d space
pattern Direction2d dX dY = Unit2d (Vector2d dX dY)

instance FFI (Direction2d FFI.Space) where
  representation = FFI.classRepresentation "Direction2d"

instance FFI (Direction2d UvSpace) where
  representation = FFI.classRepresentation "UvDirection"

instance ApproximateEquality (Direction2d space) Radians where
  d1 ~= d2 = Angle.atan2 (d1 `cross` d2) (d1 `dot` d2) ~= Angle.zero

instance Negation (Direction2d space) where
  negative (Unit2d v) = Unit2d (negative v)

instance Multiplication# Sign (Direction2d space) (Direction2d space) where
  Positive #*# direction = direction
  Negative #*# direction = negative direction

instance Multiplication Sign (Direction2d space) (Direction2d space) where
  Positive * direction = direction
  Negative * direction = negative direction

instance Multiplication# (Direction2d space) Sign (Direction2d space) where
  direction #*# Positive = direction
  direction #*# Negative = negative direction

instance Multiplication (Direction2d space) Sign (Direction2d space) where
  direction * Positive = direction
  direction * Negative = negative direction

instance Multiplication (Quantity units) (Direction2d space) (Vector2d (space @ units)) where
  scale * Unit2d v = scale * v

instance Multiplication (Direction2d space) (Quantity units) (Vector2d (space @ units)) where
  Unit2d v * scale = v * scale

instance space1 ~ space2 => DotMultiplication (Direction2d space1) (Direction2d space2) Number where
  Unit2d v1 `dot` Unit2d v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Direction2d space2) Number
  where
  Unit2d v1 `cross` Unit2d v2 = v1 `cross` v2

instance HasField "xComponent" (Direction2d space) Number where
  getField (Unit2d v) = v.xComponent

instance HasField "yComponent" (Direction2d space) Number where
  getField (Unit2d v) = v.yComponent

instance HasField "components" (Direction2d space) (Number, Number) where
  getField (Unit2d v) = v.components

instance HasField "angle" (Direction2d space) Angle where
  getField (Unit2d v) = v.angle

----- Orientation2d -----

type role Orientation2d nominal

type Orientation2d :: Type -> Type
data Orientation2d space where
  Orientation2d :: Direction2d space -> Direction2d space -> Orientation2d space

instance HasField "xDirection" (Orientation2d space) (Direction2d space) where
  getField (Orientation2d dx _) = dx

instance HasField "yDirection" (Orientation2d space) (Direction2d space) where
  getField (Orientation2d _ dy) = dy

deriving instance Eq (Orientation2d space)

deriving instance Show (Orientation2d space)

----- Point2d -----

newtype Point2d (coordinateSystem :: CoordinateSystem) = Position2d (Vector2d coordinateSystem)

{-# COMPLETE Point2d #-}

{-# INLINE Point2d #-}

-- | Construct a point from its X and Y coordinates.
pattern Point2d :: Quantity units -> Quantity units -> Point2d (space @ units)
pattern Point2d px py <- Position2d (Vector2d px py)
  where
    Point2d px py = Position2d (Vector2d px py)

instance HasField "xCoordinate" (Point2d (space @ units)) (Quantity units) where
  getField (Point2d px _) = px

instance HasField "yCoordinate" (Point2d (space @ units)) (Quantity units) where
  getField (Point2d _ py) = py

instance HasField "coordinates" (Point2d (space @ units)) (Quantity units, Quantity units) where
  getField (Point2d px py) = (px, py)

deriving instance Eq (Point2d (space @ units))

deriving instance Ord (Point2d (space @ units))

deriving instance Show (Quantity units) => Show (Point2d (space @ units))

instance FFI (Point2d FFI.Coordinates) where
  representation = FFI.classRepresentation "Point2d"

instance FFI (Point2d UvCoordinates) where
  representation = FFI.classRepresentation "UvPoint"

instance HasUnits (Point2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Point2d (space1 @ unitsA)) (Point2d (space2 @ unitsB))
  where
  coerce (Position2d vector) = Position2d (Units.coerce vector)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Position2d p + v = Position2d (p + v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Position2d p - v = Position2d (p - v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Position2d p1 - Position2d p2 = p1 - p2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Position2d p + vb = PositionBounds2d (p + vb)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Position2d p - vb = PositionBounds2d (p - vb)

instance ApproximateEquality (Point2d (space @ units)) units where
  Position2d p1 ~= Position2d p2 = p1 ~= p2

----- VectorBounds2d -----

data VectorBounds2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a vector bounding box from its X and Y coordinate bounds.
  VectorBounds2d :: Bounds units -> Bounds units -> VectorBounds2d (space @ units)

deriving instance Show (Quantity units) => Show (VectorBounds2d (space @ units))

instance HasUnits (VectorBounds2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds2d (space1 @ unitsA)) (VectorBounds2d (space2 @ unitsB))
  where
  coerce (VectorBounds2d x y) = VectorBounds2d (Units.coerce x) (Units.coerce y)

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
  box ^ point = point ^ box

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (VectorBounds2d (space1 @ units1)) (VectorBounds2d (space2 @ units2)) units1
  where
  VectorBounds2d x1 y1 ^ VectorBounds2d x2 y2 = x1 ^ x2 && y1 ^ y2

instance Negation (VectorBounds2d (space @ units)) where
  negative (VectorBounds2d x y) = VectorBounds2d (negative x) (negative y)

instance Multiplication Sign (VectorBounds2d (space @ units)) (VectorBounds2d (space @ units)) where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = negative vectorBounds

instance Multiplication (VectorBounds2d (space @ units)) Sign (VectorBounds2d (space @ units)) where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = negative vectorBounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  VectorBounds2d x1 y1 + VectorBounds2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  VectorBounds2d x1 y1 + Vector2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Vector2d x1 y1 + VectorBounds2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  VectorBounds2d x1 y1 - VectorBounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  VectorBounds2d x1 y1 - Vector2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Vector2d x1 y1 - VectorBounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  Multiplication#
    (Quantity units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ (units1 #*# units2)))
  where
  value #*# VectorBounds2d x y = VectorBounds2d (value #*# x) (value #*# y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ units3))
  where
  value * VectorBounds2d x y = VectorBounds2d (value * x) (value * y)

instance
  Multiplication#
    (VectorBounds2d (space @ units1))
    (Quantity units2)
    (VectorBounds2d (space @ (units1 #*# units2)))
  where
  VectorBounds2d x y #*# value = VectorBounds2d (x #*# value) (y #*# value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds2d (space @ units1))
    (Quantity units2)
    (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * value = VectorBounds2d (x * value) (y * value)

instance
  Multiplication#
    (Bounds units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ (units1 #*# units2)))
  where
  bounds #*# VectorBounds2d x y = VectorBounds2d (bounds #*# x) (bounds #*# y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Bounds units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ units3))
  where
  bounds * VectorBounds2d x y = VectorBounds2d (bounds * x) (bounds * y)

instance
  Multiplication#
    (VectorBounds2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ (units1 #*# units2)))
  where
  VectorBounds2d x y #*# bounds = VectorBounds2d (x #*# bounds) (y #*# bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * bounds = VectorBounds2d (x * bounds) (y * bounds)

instance
  Division#
    (VectorBounds2d (space @ units1))
    (Quantity units2)
    (VectorBounds2d (space @ (units1 #/# units2)))
  where
  VectorBounds2d x y #/# value = VectorBounds2d (x #/# value) (y #/# value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2d (space @ units1)) (Quantity units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y / value = VectorBounds2d (x / value) (y / value)

instance
  Division#
    (VectorBounds2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ (units1 #/# units2)))
  where
  VectorBounds2d x y #/# bounds = VectorBounds2d (x #/# bounds) (y #/# bounds)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2d (space @ units1)) (Bounds units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y / bounds = VectorBounds2d (x / bounds) (y / bounds)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (VectorBounds2d (space2 @ units2)) (Bounds units3)
  where
  Vector2d x1 y1 `dot` VectorBounds2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication#
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  Vector2d x1 y1 `dot#` VectorBounds2d x2 y2 = x1 #*# x2 + y1 #*# y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorBounds2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Bounds units3)
  where
  VectorBounds2d x1 y1 `dot` Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  VectorBounds2d x1 y1 `dot#` Vector2d x2 y2 = x1 #*# x2 + y1 #*# y2

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (VectorBounds2d (space2 @ units)) (Bounds units)
  where
  Unit2d vector `dot` vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds2d (space1 @ units)) (Direction2d space2) (Bounds units)
  where
  vectorBounds `dot` Unit2d vector = vectorBounds `dot` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds units3)
  where
  VectorBounds2d x1 y1 `dot` VectorBounds2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  VectorBounds2d x1 y1 `dot#` VectorBounds2d x2 y2 = x1 #*# x2 + y1 #*# y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds units3)
  where
  Vector2d x1 y1 `cross` VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  Vector2d x1 y1 `cross#` VectorBounds2d x2 y2 = x1 #*# y2 - y1 #*# x2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds units3)
  where
  VectorBounds2d x1 y1 `cross` Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  VectorBounds2d x1 y1 `cross#` Vector2d x2 y2 = x1 #*# y2 - y1 #*# x2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (VectorBounds2d (space2 @ units)) (Bounds units)
  where
  Unit2d vector `cross` vectorBounds = vector `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorBounds2d (space1 @ units)) (Direction2d space2) (Bounds units)
  where
  vectorBounds `cross` Unit2d vector = vectorBounds `cross` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds units3)
  where
  VectorBounds2d x1 y1 `cross` VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  VectorBounds2d x1 y1 `cross#` VectorBounds2d x2 y2 = x1 #*# y2 - y1 #*# x2

----- Bounds2d -----

newtype Bounds2d (coordinateSystem :: CoordinateSystem)
  = PositionBounds2d (VectorBounds2d coordinateSystem)

{-# COMPLETE Bounds2d #-}

{-# INLINE Bounds2d #-}

-- | Construct a bounding box from its X and Y coordinate bounds.
pattern Bounds2d :: Bounds units -> Bounds units -> Bounds2d (space @ units)
pattern Bounds2d bx by <- PositionBounds2d (VectorBounds2d bx by)
  where
    Bounds2d bx by = PositionBounds2d (VectorBounds2d bx by)

instance HasField "xCoordinate" (Bounds2d (space @ units)) (Bounds units) where
  getField (Bounds2d bx _) = bx

instance HasField "yCoordinate" (Bounds2d (space @ units)) (Bounds units) where
  getField (Bounds2d _ by) = by

instance HasField "coordinates" (Bounds2d (space @ units)) (Bounds units, Bounds units) where
  getField (Bounds2d bx by) = (bx, by)

deriving instance Show (Quantity units) => Show (Bounds2d (space @ units))

instance HasUnits (Bounds2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds2d (space1 @ unitsA)) (Bounds2d (space2 @ unitsB))
  where
  coerce (PositionBounds2d pb) = PositionBounds2d (Units.coerce pb)

instance FFI (Bounds2d FFI.Coordinates) where
  representation = FFI.classRepresentation "Bounds2d"

instance FFI (Bounds2d UvCoordinates) where
  representation = FFI.classRepresentation "UvBounds"

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Position2d p - PositionBounds2d pb = p - pb

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  PositionBounds2d pb - Position2d p = pb - p

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  PositionBounds2d pb1 - PositionBounds2d pb2 = pb1 - pb2

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  PositionBounds2d pb + v = PositionBounds2d (pb + v)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  PositionBounds2d pb + vb = PositionBounds2d (pb + vb)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  PositionBounds2d pb - v = PositionBounds2d (pb - v)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  PositionBounds2d pb - vb = PositionBounds2d (pb - vb)

instance
  (units1 ~ units2, space1 ~ space2) =>
  Intersects (Point2d (space1 @ units1)) (Axis2d (space2 @ units2)) units1
  where
  p ^ (Axis2d p0 d) = (p - p0) `cross` d ~= Quantity.zero

instance
  (units1 ~ units2, space1 ~ space2) =>
  Intersects (Axis2d (space2 @ units2)) (Point2d (space1 @ units1)) units1
  where
  axis ^ point = point ^ axis

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Position2d p ^ PositionBounds2d pb = p ^ pb

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  box ^ point = point ^ box

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Bounds2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  PositionBounds2d pb1 ^ PositionBounds2d pb2 = pb1 ^ pb2

----- Axis2d -----

-- | An axis in 2D, defined by an origin point and direction.
data Axis2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct an axis from its origin point and direction.
  Axis2d ::
    Point2d (space @ units) ->
    Direction2d space ->
    Axis2d (space @ units)

instance HasField "originPoint" (Axis2d (space @ units)) (Point2d (space @ units)) where
  getField (Axis2d p _) = p

instance HasField "direction" (Axis2d (space @ units)) (Direction2d space) where
  getField (Axis2d _ d) = d

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Quantity units) => Show (Axis2d (space @ units))

instance HasUnits (Axis2d (space @ units)) units

instance FFI (Axis2d FFI.Coordinates) where
  representation = FFI.classRepresentation "Axis2d"

instance FFI (Axis2d UvCoordinates) where
  representation = FFI.classRepresentation "UvAxis"

----- Frame2d -----

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d :: Point2d (space @ units) -> Orientation2d space -> Frame2d (space @ units) defines

instance HasField "originPoint" (Frame2d (space @ units) defines) (Point2d (space @ units)) where
  getField (Frame2d p _) = p

instance HasField "orientation" (Frame2d (space @ units) defines) (Orientation2d space) where
  getField (Frame2d _ o) = o

instance HasField "xDirection" (Frame2d (space @ units) defines) (Direction2d space) where
  getField = (.orientation.xDirection)

instance HasField "yDirection" (Frame2d (space @ units) defines) (Direction2d space) where
  getField = (.orientation.yDirection)

deriving instance Eq (Frame2d (space @ units) defines)

deriving instance Show (Quantity units) => Show (Frame2d (space @ units) defines)

instance FFI (Frame2d FFI.Coordinates defines) where
  representation = FFI.classRepresentation "Frame2d"

instance FFI (Frame2d UvCoordinates defines) where
  representation = FFI.classRepresentation "UvFrame"

----- Transform2d -----

type role Transform2d phantom nominal

type Transform2d :: Type -> CoordinateSystem -> Type
data Transform2d tag (coordinateSystem :: CoordinateSystem) where
  Transform2d ::
    Point2d (space @ units) ->
    Vector2d (space @ Unitless) ->
    Vector2d (space @ Unitless) ->
    Transform2d tag (space @ units)

deriving instance Eq (Transform2d tag (space @ units))

deriving instance Ord (Transform2d tag (space @ units))

deriving instance Show (Quantity units) => Show (Transform2d tag (space @ units))

instance HasUnits (Transform2d tag (space @ units)) units

instance
  (tag1 ~ tag2, space1 ~ space2) =>
  Units.Coercion
    (Transform2d tag1 (space1 @ unitsA))
    (Transform2d tag2 (space2 @ unitsB))
  where
  coerce (Transform2d p0 vx vy) = Transform2d (Units.coerce p0) vx vy

instance
  space1 ~ space2 =>
  Multiplication
    (Transform2d tag (space1 @ translationUnits))
    (Vector2d (space2 @ units))
    (Vector2d (space1 @ units))
  where
  transform * vector = vector * transform

instance
  space1 ~ space2 =>
  Multiplication
    (Vector2d (space1 @ units))
    (Transform2d tag (space2 @ translationUnits))
    (Vector2d (space1 @ units))
  where
  Vector2d vx vy * Transform2d _ i j = vx * i + vy * j

instance
  (space1 ~ space2, units1 ~ units2) =>
  Multiplication
    (Point2d (space1 @ units1))
    (Transform2d tag (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Point2d px py * Transform2d p0 i j = p0 + px * i + py * j

instance
  (space1 ~ space2, units1 ~ units2) =>
  Multiplication
    (Transform2d tag (space1 @ units1))
    (Point2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  transform * point = point * transform

instance
  (Composition tag1 tag2 tag3, space1 ~ space2, units1 ~ units2) =>
  Composition
    (Transform2d tag1 (space1 @ units1))
    (Transform2d tag2 (space2 @ units2))
    (Transform2d tag3 (space1 @ units1))
  where
  transform2 . transform1 =
    Transform2d
      @ Point2d Quantity.zero Quantity.zero * transform1 * transform2
      @ Vector2d 1.0 0.0 * transform1 * transform2
      @ Vector2d 0.0 1.0 * transform1 * transform2

----- Vector3d -----

data Vector3d (coordinateSystem :: CoordinateSystem) = Vector3d## Double# Double# Double#
  deriving (Eq, Ord, Show)

-- | Construct a vector from its X and Y components.
{-# INLINE Vector3d #-}
pattern Vector3d :: Quantity units -> Quantity units -> Quantity units -> Vector3d (space @ units)
pattern Vector3d vx vy vz <- (viewVector3d -> (# vx, vy, vz #))
  where
    Vector3d (Quantity## vx##) (Quantity## vy##) (Quantity## vz##) = Vector3d## vx## vy## vz##

{-# INLINE viewVector3d #-}
viewVector3d :: Vector3d (space @ units) -> (# Quantity units, Quantity units, Quantity units #)
viewVector3d (Vector3d## vx## vy## vz##) = (# Quantity## vx##, Quantity## vy##, Quantity## vz## #)

{-# COMPLETE Vector3d #-}

instance FFI (Vector3d (FFI.Space @ Unitless)) where
  representation = FFI.classRepresentation "Vector3d"

instance FFI (Vector3d (FFI.Space @ Meters)) where
  representation = FFI.classRepresentation "Displacement3d"

instance FFI (Vector3d (FFI.Space @ SquareMeters)) where
  representation = FFI.classRepresentation "AreaVector3d"

instance HasUnits (Vector3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector3d (space1 @ unitsA))
    (Vector3d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance ApproximateEquality (Vector3d (space @ units)) units where
  Vector3d x1 y1 z1 ~= Vector3d x2 y2 z2 =
    Quantity.hypot3 (x2 - x1) (y2 - y1) (z2 - z1) ~= Quantity.zero

instance HasZero (Vector3d (space @ units)) where
  zero = Vector3d Quantity.zero Quantity.zero Quantity.zero

instance Negation (Vector3d (space @ units)) where
  negative (Vector3d vx vy vz) = Vector3d (negative vx) (negative vy) (negative vz)

instance Multiplication Sign (Vector3d (space @ units)) (Vector3d (space @ units)) where
  Positive * vector = vector
  Negative * vector = negative vector

instance Multiplication (Vector3d (space @ units)) Sign (Vector3d (space @ units)) where
  vector * Positive = vector
  vector * Negative = negative vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Vector3d (space1 @ units1))
  where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Vector3d (space1 @ units1))
  where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  Multiplication#
    (Quantity units1)
    (Vector3d (space @ units2))
    (Vector3d (space @ (units1 #*# units2)))
  where
  scale #*# Vector3d vx vy vz = Vector3d (scale #*# vx) (scale #*# vy) (scale #*# vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Vector3d (space @ units2)) (Vector3d (space @ units3))
  where
  scale * Vector3d vx vy vz = Vector3d (scale * vx) (scale * vy) (scale * vz)

instance
  Multiplication#
    (Vector3d (space @ units1))
    (Quantity units2)
    (Vector3d (space @ (units1 #*# units2)))
  where
  Vector3d vx vy vz #*# scale = Vector3d (vx #*# scale) (vy #*# scale) (vz #*# scale)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Quantity units2) (Vector3d (space @ units3))
  where
  Vector3d vx vy vz * scale = Vector3d (vx * scale) (vy * scale) (vz * scale)

instance
  Multiplication#
    (Bounds units1)
    (Vector3d (space @ units2))
    (VectorBounds3d (space @ (units1 #*# units2)))
  where
  bounds #*# Vector3d vx vy vz = VectorBounds3d (bounds #*# vx) (bounds #*# vy) (bounds #*# vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Vector3d (space @ units2)) (VectorBounds3d (space @ units3))
  where
  bounds * Vector3d vx vy vz = VectorBounds3d (bounds * vx) (bounds * vy) (bounds * vz)

instance
  Multiplication#
    (Vector3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ (units1 #*# units2)))
  where
  Vector3d vx vy vz #*# bounds = VectorBounds3d (vx #*# bounds) (vy #*# bounds) (vz #*# bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Bounds units2) (VectorBounds3d (space @ units3))
  where
  Vector3d vx vy vz * bounds = VectorBounds3d (vx * bounds) (vy * bounds) (vz * bounds)

instance
  Division#
    (Vector3d (space @ units1))
    (Quantity units2)
    (Vector3d (space @ (units1 #/# units2)))
  where
  Vector3d vx vy vz #/# scale = Vector3d (vx #/# scale) (vy #/# scale) (vz #/# scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector3d (space @ units1)) (Quantity units2) (Vector3d (space @ units3))
  where
  Vector3d vx vy vz / scale = Vector3d (vx / scale) (vy / scale) (vz / scale)

instance
  space1 ~ space2 =>
  DotMultiplication#
    (Vector3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Quantity (units1 #*# units2))
  where
  Vector3d x1 y1 z1 `dot#` Vector3d x2 y2 z2 = x1 #*# x2 + y1 #*# y2 + z1 #*# z2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d (space1 @ units1)) (Vector3d (space2 @ units2)) (Quantity units3)
  where
  Vector3d x1 y1 z1 `dot` Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (Direction3d space2) (Quantity units)
  where
  v1 `dot` Unit3d v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (Vector3d (space2 @ units)) (Quantity units)
  where
  Unit3d v1 `dot` v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (Vector3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Vector3d (space1 @ (units1 #*# units2)))
  where
  Vector3d x1 y1 z1 `cross#` Vector3d x2 y2 z2 =
    Vector3d
      (y1 #*# z2 - z1 #*# y2)
      (z1 #*# x2 - x1 #*# z2)
      (x1 #*# y2 - y1 #*# x2)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Vector3d (space1 @ units3))
  where
  Vector3d x1 y1 z1 `cross` Vector3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector3d (space1 @ units)) (Direction3d space2) (Vector3d (space1 @ units))
  where
  v1 `cross` Unit3d v2 = v1 `cross` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction3d space1) (Vector3d (space2 @ units)) (Vector3d (space1 @ units))
  where
  Unit3d v1 `cross` v2 = v1 `cross` v2

----- Direction3d -----

{-| A direction in 3D.

This is effectively a type-safe unit vector.
-}
newtype Direction3d (space :: Type) = Unit3d (Vector3d (space @ Unitless))
  deriving (Eq, Ord, Show)

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Number -> Number -> Number -> Direction3d space
pattern Direction3d dR dF dU = Unit3d (Vector3d dR dF dU)

instance FFI (Direction3d FFI.Space) where
  representation = FFI.classRepresentation "Direction3d"

instance ApproximateEquality (Direction3d space) Radians where
  d1 ~= d2 = do
    let parallel = d1 `dot` d2
    let Vector3d cx cy cz = d1 `cross` d2
    let perpendicular = Quantity.hypot3 cx cy cz
    Angle.atan2 perpendicular parallel ~= Quantity.zero

instance Negation (Direction3d space) where
  negative (Unit3d vector) = Unit3d (negative vector)

instance Multiplication Sign (Direction3d space) (Direction3d space) where
  Positive * direction = direction
  Negative * direction = negative direction

instance Multiplication# Sign (Direction3d space) (Direction3d space) where
  Positive #*# direction = direction
  Negative #*# direction = negative direction

instance Multiplication (Direction3d space) Sign (Direction3d space) where
  direction * Positive = direction
  direction * Negative = negative direction

instance Multiplication# (Direction3d space) Sign (Direction3d space) where
  direction #*# Positive = direction
  direction #*# Negative = negative direction

instance Multiplication (Quantity units) (Direction3d space) (Vector3d (space @ units)) where
  scale * Unit3d vector = scale * vector

instance Multiplication (Direction3d space) (Quantity units) (Vector3d (space @ units)) where
  Unit3d vector * scale = vector * scale

instance Multiplication (Bounds units) (Direction3d space) (VectorBounds3d (space @ units)) where
  bounds * Unit3d vector = bounds * vector

instance Multiplication (Direction3d space) (Bounds units) (VectorBounds3d (space @ units)) where
  Unit3d vector * bounds = vector * bounds

instance space1 ~ space2 => DotMultiplication (Direction3d space1) (Direction3d space2) Number where
  Unit3d vector1 `dot` Unit3d vector2 = vector1 `dot` vector2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction3d space1) (Direction3d space2) (Vector3d (space1 @ Unitless))
  where
  Unit3d vector1 `cross` Unit3d vector2 = vector1 `cross` vector2

----- PlaneOrientation3d -----

-- | A pair of perpendicular X and Y directions defining the orientation of a plane in 3D.
type PlaneOrientation3d :: Type -> Type
data PlaneOrientation3d space where
  PlaneOrientation3d :: Direction3d space -> Direction3d space -> PlaneOrientation3d space

deriving instance Eq (PlaneOrientation3d space)

deriving instance Ord (PlaneOrientation3d space)

deriving instance Show (PlaneOrientation3d space)

instance FFI (PlaneOrientation3d FFI.Space) where
  representation = FFI.classRepresentation "PlaneOrientation3d"

instance HasField "xDirection" (PlaneOrientation3d space) (Direction3d space) where
  getField (PlaneOrientation3d dx _) = dx

instance HasField "yDirection" (PlaneOrientation3d space) (Direction3d space) where
  getField (PlaneOrientation3d _ dy) = dy

instance HasField "normalDirection" (PlaneOrientation3d space) (Direction3d space) where
  getField (PlaneOrientation3d dx dy) = Unit3d (dx `cross` dy)

----- Orientation3d -----

-- | A set of cardinal directions (forward, upward etc.) defining a 3D orientation.
type Orientation3d :: Type -> Type
data Orientation3d space where
  Orientation3d ::
    Direction3d space ->
    Direction3d space ->
    Direction3d space ->
    Orientation3d space

deriving instance Eq (Orientation3d space)

deriving instance Show (Orientation3d space)

instance FFI (Orientation3d FFI.Space) where
  representation = FFI.classRepresentation "Orientation3d"

instance HasField "rightwardDirection" (Orientation3d space) (Direction3d space) where
  getField (Orientation3d r _ _) = r

instance HasField "leftwardDirection" (Orientation3d space) (Direction3d space) where
  getField orientation = negative orientation.rightwardDirection

instance HasField "forwardDirection" (Orientation3d space) (Direction3d space) where
  getField (Orientation3d _ f _) = f

instance HasField "backwardDirection" (Orientation3d space) (Direction3d space) where
  getField orientation = negative orientation.forwardDirection

instance HasField "upwardDirection" (Orientation3d space) (Direction3d space) where
  getField (Orientation3d _ _ u) = u

instance HasField "downwardDirection" (Orientation3d space) (Direction3d space) where
  getField orientation = negative orientation.upwardDirection

instance HasField "rightPlaneOrientation" (Orientation3d space) (PlaneOrientation3d space) where
  getField orientation =
    PlaneOrientation3d orientation.forwardDirection orientation.upwardDirection

instance HasField "leftPlaneOrientation" (Orientation3d space) (PlaneOrientation3d space) where
  getField orientation =
    PlaneOrientation3d orientation.backwardDirection orientation.upwardDirection

instance HasField "frontPlaneOrientation" (Orientation3d space) (PlaneOrientation3d space) where
  getField orientation =
    PlaneOrientation3d orientation.leftwardDirection orientation.upwardDirection

instance HasField "backPlaneOrientation" (Orientation3d space) (PlaneOrientation3d space) where
  getField orientation =
    PlaneOrientation3d orientation.rightwardDirection orientation.upwardDirection

instance HasField "topPlaneOrientation" (Orientation3d space) (PlaneOrientation3d space) where
  getField orientation =
    PlaneOrientation3d orientation.rightwardDirection orientation.forwardDirection

instance HasField "bottomPlaneOrientation" (Orientation3d space) (PlaneOrientation3d space) where
  getField orientation =
    PlaneOrientation3d orientation.leftwardDirection orientation.forwardDirection

instance HasField "backwardOrientation" (Orientation3d space) (Orientation3d space) where
  getField orientation =
    Orientation3d
      orientation.leftwardDirection
      orientation.backwardDirection
      orientation.upwardDirection

instance HasField "rightwardOrientation" (Orientation3d space) (Orientation3d space) where
  getField orientation =
    Orientation3d
      orientation.backwardDirection
      orientation.rightwardDirection
      orientation.upwardDirection

instance HasField "leftwardOrientation" (Orientation3d space) (Orientation3d space) where
  getField orientation =
    Orientation3d
      orientation.forwardDirection
      orientation.leftwardDirection
      orientation.upwardDirection

instance HasField "upwardOrientation" (Orientation3d space) (Orientation3d space) where
  getField orientation =
    Orientation3d
      orientation.leftwardDirection
      orientation.upwardDirection
      orientation.forwardDirection

instance HasField "downwardOrientation" (Orientation3d space) (Orientation3d space) where
  getField orientation =
    Orientation3d
      orientation.rightwardDirection
      orientation.downwardDirection
      orientation.forwardDirection

----- Point3d -----

newtype Point3d (coordinateSystem :: CoordinateSystem) = Position3d (Vector3d coordinateSystem)

{-# COMPLETE Point3d #-}

{-# INLINE Point3d #-}

-- | Construct a point from its X and Y coordinates.
pattern Point3d :: Quantity units -> Quantity units -> Quantity units -> Point3d (space @ units)
pattern Point3d px py pz <- Position3d (Vector3d px py pz)
  where
    Point3d px py pz = Position3d (Vector3d px py pz)

deriving instance Eq (Point3d (space @ units))

deriving instance Ord (Point3d (space @ units))

deriving instance Show (Quantity units) => Show (Point3d (space @ units))

instance FFI (Point3d FFI.Coordinates) where
  representation = FFI.classRepresentation "Point3d"

instance HasUnits (Point3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Point3d (space1 @ unitsA)) (Point3d (space2 @ unitsB))
  where
  coerce (Position3d p) = Position3d (Units.coerce p)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Position3d p + v = Position3d (p + v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Position3d p - v = Position3d (p - v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (Vector3d (space1 @ units1))
  where
  Position3d p1 - Position3d p2 = p1 - p2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Position3d p + vb = PositionBounds3d (p + vb)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Position3d p - vb = PositionBounds3d (p - vb)

instance ApproximateEquality (Point3d (space @ units)) units where
  Position3d p1 ~= Position3d p2 = p1 ~= p2

----- VectorBounds3d -----

data VectorBounds3d (coordinateSystem :: CoordinateSystem)
  = VectorBounds3d## Double# Double# Double# Double# Double# Double#
  deriving (Show)

-- | Construct a vector bounds from its rightward, forward and upward components.
{-# INLINE VectorBounds3d #-}
pattern VectorBounds3d ::
  Bounds units ->
  Bounds units ->
  Bounds units ->
  VectorBounds3d (space @ units)
pattern VectorBounds3d x y z <- (viewVectorBounds3d -> (# x, y, z #))
  where
    VectorBounds3d (Bounds## xl## xh##) (Bounds## yl## yh##) (Bounds## zl## zh##) =
      VectorBounds3d## xl## xh## yl## yh## zl## zh##

viewVectorBounds3d ::
  VectorBounds3d (space @ units) ->
  (# Bounds units, Bounds units, Bounds units #)
viewVectorBounds3d (VectorBounds3d## xl## xh## yl## yh## zl## zh##) =
  (# Bounds## xl## xh##, Bounds## yl## yh##, Bounds## zl## zh## #)

{-# COMPLETE VectorBounds3d #-}

instance HasUnits (VectorBounds3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds3d (space1 @ unitsA)) (VectorBounds3d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Vector3d (space1 @ units1)) (VectorBounds3d (space2 @ units2)) units1
  where
  Vector3d vR vF vU ^ VectorBounds3d bR bF bU = vR ^ bR && vF ^ bF && vU ^ bU

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (VectorBounds3d (space1 @ units1)) (Vector3d (space2 @ units2)) units1
  where
  box ^ point = point ^ box

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (VectorBounds3d (space1 @ units1)) (VectorBounds3d (space2 @ units2)) units1
  where
  VectorBounds3d r1 f1 u1 ^ VectorBounds3d r2 f2 u2 = r1 ^ r2 && f1 ^ f2 && u1 ^ u2

instance Negation (VectorBounds3d (space @ units)) where
  negative (VectorBounds3d## xl## xh## yl## yh## zl## zh##) = do
    VectorBounds3d##
      (negate## xh##)
      (negate## xl##)
      (negate## yh##)
      (negate## yl##)
      (negate## zh##)
      (negate## zl##)

instance
  Multiplication
    Sign
    (VectorBounds3d (space @ units))
    (VectorBounds3d (space @ units))
  where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = negative vectorBounds

instance
  Multiplication
    (VectorBounds3d (space @ units))
    Sign
    (VectorBounds3d (space @ units))
  where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = negative vectorBounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  VectorBounds3d## xl1## xh1## yl1## yh1## zl1## zh1##
    + VectorBounds3d## xl2## xh2## yl2## yh2## zl2## zh2## = do
      let !(# xl##, xh## #) = boundsPlusBounds## xl1## xh1## xl2## xh2##
      let !(# yl##, yh## #) = boundsPlusBounds## yl1## yh1## yl2## yh2##
      let !(# zl##, zh## #) = boundsPlusBounds## zl1## zh1## zl2## zh2##
      VectorBounds3d## xl## xh## yl## yh## zl## zh##

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  VectorBounds3d x1 y1 z1 + Vector3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  Vector3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  VectorBounds3d## xl1## xh1## yl1## yh1## zl1## zh1##
    - VectorBounds3d## xl2## xh2## yl2## yh2## zl2## zh2## = do
      let !(# xl##, xh## #) = boundsMinusBounds## xl1## xh1## xl2## xh2##
      let !(# yl##, yh## #) = boundsMinusBounds## yl1## yh1## yl2## yh2##
      let !(# zl##, zh## #) = boundsMinusBounds## zl1## zh1## zl2## zh2##
      VectorBounds3d## xl## xh## yl## yh## zl## zh##

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  VectorBounds3d x1 y1 z1 - Vector3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  Vector3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

{-# INLINE quantityTimesVectorBounds3d #-}
quantityTimesVectorBounds3d ::
  Quantity units1 ->
  VectorBounds3d (space @ units2) ->
  VectorBounds3d (space @ units3)
quantityTimesVectorBounds3d
  (Quantity## v1##)
  (VectorBounds3d## xl2## xh2## yl2## yh2## zl2## zh2##) = do
    let !(# xl##, xh## #) = doubleTimesBounds## v1## xl2## xh2##
    let !(# yl##, yh## #) = doubleTimesBounds## v1## yl2## yh2##
    let !(# zl##, zh## #) = doubleTimesBounds## v1## zl2## zh2##
    VectorBounds3d## xl## xh## yl## yh## zl## zh##

instance
  Multiplication#
    (Quantity units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ (units1 #*# units2)))
  where
  lhs #*# rhs = quantityTimesVectorBounds3d lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ units3))
  where
  lhs * rhs = quantityTimesVectorBounds3d lhs rhs

instance
  Multiplication#
    (VectorBounds3d (space @ units1))
    (Quantity units2)
    (VectorBounds3d (space @ (units1 #*# units2)))
  where
  lhs #*# rhs = quantityTimesVectorBounds3d rhs lhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds3d (space @ units1))
    (Quantity units2)
    (VectorBounds3d (space @ units3))
  where
  lhs * rhs = quantityTimesVectorBounds3d rhs lhs

{-# INLINE boundsTimesVectorBounds3d #-}
boundsTimesVectorBounds3d ::
  Bounds units1 ->
  VectorBounds3d (space @ units2) ->
  VectorBounds3d (space @ units3)
boundsTimesVectorBounds3d
  (Bounds## vl1## vh1##)
  (VectorBounds3d## xl2## xh2## yl2## yh2## zl2## zh2##) = do
    let !(# xl##, xh## #) = boundsTimesBounds## vl1## vh1## xl2## xh2##
    let !(# yl##, yh## #) = boundsTimesBounds## vl1## vh1## yl2## yh2##
    let !(# zl##, zh## #) = boundsTimesBounds## vl1## vh1## zl2## zh2##
    VectorBounds3d## xl## xh## yl## yh## zl## zh##

instance
  Multiplication#
    (Bounds units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ (units1 #*# units2)))
  where
  lhs #*# rhs = boundsTimesVectorBounds3d lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Bounds units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ units3))
  where
  lhs * rhs = boundsTimesVectorBounds3d lhs rhs

instance
  Multiplication#
    (VectorBounds3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ (units1 #*# units2)))
  where
  lhs #*# rhs = boundsTimesVectorBounds3d rhs lhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ units3))
  where
  lhs * rhs = boundsTimesVectorBounds3d rhs lhs

instance
  Division#
    (VectorBounds3d (space @ units1))
    (Quantity units2)
    (VectorBounds3d (space @ (units1 #/# units2)))
  where
  VectorBounds3d x y z #/# value = VectorBounds3d (x #/# value) (y #/# value) (z #/# value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3d (space @ units1)) (Quantity units2) (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z / value = VectorBounds3d (x / value) (y / value) (z / value)

instance
  Division#
    (VectorBounds3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ (units1 #/# units2)))
  where
  VectorBounds3d x y z #/# bounds = VectorBounds3d (x #/# bounds) (y #/# bounds) (z #/# bounds)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3d (space @ units1)) (Bounds units2) (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z / bounds = VectorBounds3d (x / bounds) (y / bounds) (z / bounds)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d (space1 @ units1)) (VectorBounds3d (space2 @ units2)) (Bounds units3)
  where
  Vector3d x1 y1 z1 `dot` VectorBounds3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space1 ~ space2 =>
  DotMultiplication#
    (Vector3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  Vector3d x1 y1 z1 `dot#` VectorBounds3d x2 y2 z2 = x1 #*# x2 + y1 #*# y2 + z1 #*# z2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorBounds3d (space1 @ units1)) (Vector3d (space2 @ units2)) (Bounds units3)
  where
  VectorBounds3d x1 y1 z1 `dot` Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorBounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  VectorBounds3d x1 y1 z1 `dot#` Vector3d x2 y2 z2 = x1 #*# x2 + y1 #*# y2 + z1 #*# z2

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (VectorBounds3d (space2 @ units)) (Bounds units)
  where
  Unit3d vector `dot` vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds3d (space1 @ units)) (Direction3d space2) (Bounds units)
  where
  vectorBounds `dot` Unit3d vector = vectorBounds `dot` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorBounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds units3)
  where
  VectorBounds3d x1 y1 z1 `dot` VectorBounds3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space1 ~ space2 =>
  DotMultiplication#
    (VectorBounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds (units1 #*# units2))
  where
  VectorBounds3d x1 y1 z1 `dot#` VectorBounds3d x2 y2 z2 = x1 #*# x2 + y1 #*# y2 + z1 #*# z2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units3))
  where
  Vector3d x1 y1 z1 `cross` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (Vector3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ (units1 #*# units2)))
  where
  Vector3d x1 y1 z1 `cross#` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 #*# z2 - z1 #*# y2)
      (z1 #*# x2 - x1 #*# z2)
      (x1 #*# y2 - y1 #*# x2)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorBounds3d (space1 @ units3))
  where
  VectorBounds3d x1 y1 z1 `cross` Vector3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorBounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorBounds3d (space1 @ (units1 #*# units2)))
  where
  VectorBounds3d x1 y1 z1 `cross#` Vector3d x2 y2 z2 =
    VectorBounds3d
      (y1 #*# z2 - z1 #*# y2)
      (z1 #*# x2 - x1 #*# z2)
      (x1 #*# y2 - y1 #*# x2)

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorBounds3d (space2 @ units))
    (VectorBounds3d (space1 @ units))
  where
  Unit3d vector `cross` vectorBounds = vector `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorBounds3d (space1 @ units))
    (Direction3d space2)
    (VectorBounds3d (space1 @ units))
  where
  vectorBounds `cross` Unit3d vector = vectorBounds `cross` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units3))
  where
  VectorBounds3d x1 y1 z1 `cross` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space1 ~ space2 =>
  CrossMultiplication#
    (VectorBounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ (units1 #*# units2)))
  where
  VectorBounds3d x1 y1 z1 `cross#` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 #*# z2 - z1 #*# y2)
      (z1 #*# x2 - x1 #*# z2)
      (x1 #*# y2 - y1 #*# x2)

----- Bounds3d -----

-- | A bounding box in 3D.
newtype Bounds3d (coordinateSystem :: CoordinateSystem)
  = PositionBounds3d (VectorBounds3d coordinateSystem)

{-# COMPLETE Bounds3d #-}

{-# INLINE Bounds3d #-}

-- | Construct a point from its X and Y coordinates.
pattern Bounds3d :: Bounds units -> Bounds units -> Bounds units -> Bounds3d (space @ units)
pattern Bounds3d bx by bz <- PositionBounds3d (VectorBounds3d bx by bz)
  where
    Bounds3d bx by bz = PositionBounds3d (VectorBounds3d bx by bz)

deriving instance Show (Quantity units) => Show (Bounds3d (space @ units))

instance FFI (Bounds3d FFI.Coordinates) where
  representation = FFI.classRepresentation "Bounds3d"

instance HasUnits (Bounds3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds3d (space1 @ unitsA)) (Bounds3d (space2 @ unitsB))
  where
  coerce (PositionBounds3d pb) = PositionBounds3d (Units.coerce pb)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  PositionBounds3d pb + v = PositionBounds3d (pb + v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  PositionBounds3d pb + vb = PositionBounds3d (pb + vb)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  PositionBounds3d pb - v = PositionBounds3d (pb - v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  PositionBounds3d pb - vb = PositionBounds3d (pb - vb)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Bounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  Position3d p - PositionBounds3d pb = p - pb

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  PositionBounds3d pb - Position3d p = pb - p

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (Bounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  PositionBounds3d pb1 - PositionBounds3d pb2 = pb1 - pb2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Point3d (space1 @ units1)) (Bounds3d (space2 @ units2)) units1
  where
  Position3d p ^ PositionBounds3d pb = p ^ pb

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Bounds3d (space1 @ units1)) (Point3d (space2 @ units2)) units1
  where
  box ^ point = point ^ box

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Bounds3d (space1 @ units1)) (Bounds3d (space2 @ units2)) units1
  where
  PositionBounds3d pb1 ^ PositionBounds3d pb2 = pb1 ^ pb2

----- Axis3d -----

-- | An axis in 3D, defined by an origin point and direction.
data Axis3d (coordinateSystem :: CoordinateSystem) where
  -- | Construct an axis from its origin point and direction.
  Axis3d ::
    Point3d (space @ units) ->
    Direction3d space ->
    Axis3d (space @ units)

instance HasField "originPoint" (Axis3d (space @ units)) (Point3d (space @ units)) where
  getField (Axis3d p _) = p

instance HasField "direction" (Axis3d (space @ units)) (Direction3d space) where
  getField (Axis3d _ d) = d

deriving instance Eq (Axis3d (space @ units))

deriving instance Show (Quantity units) => Show (Axis3d (space @ units))

instance FFI (Axis3d FFI.Coordinates) where
  representation = FFI.classRepresentation "Axis3d"

----- Plane3d -----

type role Plane3d nominal nominal

type Plane3d :: CoordinateSystem -> LocalSpace -> Type

{-| A plane in 3D, defined by an origin point and two perpendicular X and Y directions.

The normal direction  of the plane is then defined as
the cross product of its X and Y directions.
-}
data Plane3d coordinateSystem defines where
  Plane3d :: Point3d (space @ units) -> PlaneOrientation3d space -> Plane3d (space @ units) defines

deriving instance Eq (Plane3d (space @ units) defines)

deriving instance Ord (Plane3d (space @ units) defines)

deriving instance Show (Quantity units) => Show (Plane3d (space @ units) defines)

instance FFI (Plane3d FFI.Coordinates defines) where
  representation = FFI.classRepresentation "Plane3d"

instance
  HasField
    "originPoint"
    (Plane3d (space @ units) defines)
    (Point3d (space @ units))
  where
  getField (Plane3d p _) = p

instance HasField "orientation" (Plane3d (space @ units) defines) (PlaneOrientation3d space) where
  getField (Plane3d _ o) = o

instance HasField "xDirection" (Plane3d (space @ units) defines) (Direction3d space) where
  getField = (.orientation.xDirection)

instance HasField "yDirection" (Plane3d (space @ units) defines) (Direction3d space) where
  getField = (.orientation.yDirection)

instance HasField "normalDirection" (Plane3d (space @ units) defines) (Direction3d space) where
  getField = (.orientation.normalDirection)

instance HasField "xAxis" (Plane3d (space @ units) defines) (Axis3d (space @ units)) where
  getField plane = Axis3d plane.originPoint plane.xDirection

instance HasField "yAxis" (Plane3d (space @ units) defines) (Axis3d (space @ units)) where
  getField plane = Axis3d plane.originPoint plane.yDirection

instance HasField "normalAxis" (Plane3d (space @ units) defines) (Axis3d (space @ units)) where
  getField plane = Axis3d plane.originPoint plane.normalDirection

----- Frame3d -----

-- | A frame of reference in 3D, defined by an origin point and orientation.
type Frame3d :: CoordinateSystem -> LocalSpace -> Type
data Frame3d coordinateSystem defines where
  Frame3d :: Point3d (space @ units) -> Orientation3d space -> Frame3d (space @ units) defines

instance
  HasField
    "originPoint"
    (Frame3d (space @ units) defines)
    (Point3d (space @ units))
  where
  getField (Frame3d p _) = p

instance HasField "orientation" (Frame3d (space @ units) defines) (Orientation3d space) where
  getField (Frame3d _ o) = o

instance
  HasField
    "rightwardDirection"
    (Frame3d (space @ units) defines)
    (Direction3d space)
  where
  getField = (.orientation.rightwardDirection)

instance
  HasField
    "leftwardDirection"
    (Frame3d (space @ units) defines)
    (Direction3d space)
  where
  getField = (.orientation.leftwardDirection)

instance
  HasField
    "forwardDirection"
    (Frame3d (space @ units) defines)
    (Direction3d space)
  where
  getField = (.orientation.forwardDirection)

instance
  HasField
    "backwardDirection"
    (Frame3d (space @ units) defines)
    (Direction3d space)
  where
  getField = (.orientation.backwardDirection)

instance
  HasField
    "upwardDirection"
    (Frame3d (space @ units) defines)
    (Direction3d space)
  where
  getField = (.orientation.upwardDirection)

instance
  HasField
    "downwardDirection"
    (Frame3d (space @ units) defines)
    (Direction3d space)
  where
  getField = (.orientation.downwardDirection)

instance
  HasField
    "rightwardAxis"
    (Frame3d (space @ units) defines)
    (Axis3d (space @ units))
  where
  getField frame = Axis3d frame.originPoint frame.rightwardDirection

instance
  HasField
    "leftwardAxis"
    (Frame3d (space @ units) defines)
    (Axis3d (space @ units))
  where
  getField frame = Axis3d frame.originPoint frame.leftwardDirection

instance
  HasField
    "forwardAxis"
    (Frame3d (space @ units) defines)
    (Axis3d (space @ units))
  where
  getField frame = Axis3d frame.originPoint frame.forwardDirection

instance
  HasField
    "backwardAxis"
    (Frame3d (space @ units) defines)
    (Axis3d (space @ units))
  where
  getField frame = Axis3d frame.originPoint frame.backwardDirection

instance
  HasField
    "upwardAxis"
    (Frame3d (space @ units) defines)
    (Axis3d (space @ units))
  where
  getField frame = Axis3d frame.originPoint frame.upwardDirection

instance
  HasField
    "downwardAxis"
    (Frame3d (space @ units) defines)
    (Axis3d (space @ units))
  where
  getField frame = Axis3d frame.originPoint frame.downwardDirection

instance
  HasField
    "backwardOrientation"
    (Frame3d (space @ units) defines)
    (Orientation3d space)
  where
  getField = (.orientation.backwardOrientation)

instance
  HasField
    "rightwardOrientation"
    (Frame3d (space @ units) defines)
    (Orientation3d space)
  where
  getField = (.orientation.rightwardOrientation)

instance
  HasField
    "leftwardOrientation"
    (Frame3d (space @ units) defines)
    (Orientation3d space)
  where
  getField = (.orientation.leftwardOrientation)

instance
  HasField
    "upwardOrientation"
    (Frame3d (space @ units) defines)
    (Orientation3d space)
  where
  getField = (.orientation.upwardOrientation)

instance
  HasField
    "downwardOrientation"
    (Frame3d (space @ units) defines)
    (Orientation3d space)
  where
  getField = (.orientation.downwardOrientation)

deriving instance Eq (Frame3d (space @ units) defines)

deriving instance Show (Quantity units) => Show (Frame3d (space @ units) defines)

instance FFI (Frame3d FFI.Coordinates defines) where
  representation = FFI.classRepresentation "Frame3d"

instance
  (space1 ~ space2, defines1 ~ defines2) =>
  Units.Coercion (Frame3d (space1 @ units1) defines1) (Frame3d (space2 @ units2) defines2)
  where
  coerce (Frame3d p o) = Frame3d (Units.coerce p) o

----- Transform3d -----

type Transform3d :: Type -> CoordinateSystem -> Type
data Transform3d tag (coordinateSystem :: CoordinateSystem) where
  Transform3d ::
    Point3d (space @ units) ->
    Vector3d (space @ Unitless) ->
    Vector3d (space @ Unitless) ->
    Vector3d (space @ Unitless) ->
    Transform3d tag (space @ units)

deriving instance Eq (Transform3d tag (space @ units))

deriving instance Ord (Transform3d tag (space @ units))

deriving instance Show (Quantity units) => Show (Transform3d tag (space @ units))

instance HasUnits (Transform3d tag (space @ units)) units

instance
  (tag1 ~ tag2, space1 ~ space2) =>
  Units.Coercion
    (Transform3d tag1 (space1 @ unitsA))
    (Transform3d tag2 (space2 @ unitsB))
  where
  coerce (Transform3d p0 vx vy vz) = Transform3d (Units.coerce p0) vx vy vz

instance
  space1 ~ space2 =>
  Multiplication
    (Transform3d tag (space1 @ translationUnits))
    (Vector3d (space2 @ units))
    (Vector3d (space1 @ units))
  where
  transform * vector = vector * transform

instance
  space1 ~ space2 =>
  Multiplication
    (Vector3d (space1 @ units))
    (Transform3d tag (space2 @ translationUnits))
    (Vector3d (space1 @ units))
  where
  Vector3d vx vy vz * Transform3d _ i j k = vx * i + vy * j + vz * k

instance
  (space1 ~ space2, units1 ~ units2) =>
  Multiplication
    (Point3d (space1 @ units1))
    (Transform3d tag (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Point3d px py pz * Transform3d p0 i j k = p0 + px * i + py * j + pz * k

instance
  (space1 ~ space2, units1 ~ units2) =>
  Multiplication
    (Transform3d tag (space1 @ units1))
    (Point3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  transform * point = point * transform

instance
  ( Composition tag1 tag2 tag3
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Composition
    (Transform3d tag1 (space1 @ units1))
    (Transform3d tag2 (space2 @ units2))
    (Transform3d tag3 (space1 @ units1))
  where
  transform2 . transform1 =
    Transform3d
      (Point3d Quantity.zero Quantity.zero Quantity.zero * transform1 * transform2)
      (Vector3d 1.0 0.0 0.0 * transform1 * transform2)
      (Vector3d 0.0 1.0 0.0 * transform1 * transform2)
      (Vector3d 0.0 0.0 1.0 * transform1 * transform2)
