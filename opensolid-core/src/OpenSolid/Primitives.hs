{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Primitives
  ( Vector2D (Vector2D, Vector2D#)
  , Direction2D (Unit2D, Direction2D)
  , Orientation2D (Orientation2D)
  , Point2D (Point2D, Position2D)
  , VectorBounds2D (VectorBounds2D)
  , Bounds2D (Bounds2D, PositionBounds2D)
  , Axis2D (Axis2D, originPoint, direction)
  , Frame2D (Frame2D, originPoint, orientation)
  , Transform2D (Transform2D)
  , Vector3D (Vector3D, Vector3D#)
  , Direction3D (Unit3D, Direction3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Orientation3D (Orientation3D)
  , Point3D (Point3D, Position3D)
  , VectorBounds3D (VectorBounds3D, VectorBounds3D#)
  , Bounds3D (Bounds3D, PositionBounds3D)
  , Axis3D (Axis3D, originPoint, direction)
  , Plane3D (Plane3D, originPoint, orientation)
  , Frame3D (Frame3D, originPoint, orientation)
  , Transform3D (Transform3D)
  )
where

import Data.Coerce qualified
import GHC.Records (HasField (getField))
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds (Bounds (Bounds#))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.HasZero (HasZero)
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.Length (Length)
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance ((~=#))
import OpenSolid.Unboxed.Math
import OpenSolid.Units (HasUnits, SquareMeters)
import OpenSolid.Units qualified as Units

----- Vector2D -----

type role Vector2D phantom phantom

type Vector2D :: Type -> Type -> Type
data Vector2D units space = Vector2D# Double# Double#
  deriving (Eq, Ord, Show)

-- | Construct a vector from its X and Y components.
{-# INLINE Vector2D #-}
pattern Vector2D :: Quantity units -> Quantity units -> Vector2D units space
pattern Vector2D vx vy <- (viewVector2D -> (# vx, vy #))
  where
    Vector2D (Quantity# vx#) (Quantity# vy#) = Vector2D# vx# vy#

{-# INLINE viewVector2D #-}
viewVector2D :: Vector2D units space -> (# Quantity units, Quantity units #)
viewVector2D (Vector2D# vx# vy#) = (# Quantity# vx#, Quantity# vy# #)

{-# COMPLETE Vector2D #-}

instance FFI (Vector2D Unitless FFI.Space) where
  representation = FFI.classRepresentation "Vector2D"

instance FFI (Vector2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Displacement2D"

instance FFI (Vector2D SquareMeters FFI.Space) where
  representation = FFI.classRepresentation "AreaVector2D"

instance FFI (Vector2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvVector"

instance HasUnits (Vector2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector2D unitsA space1)
    (Vector2D unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance ApproximateEquality (Vector2D units space) units where
  Vector2D# x1# y1# ~= Vector2D# x2# y2# =
    case hypot2# (x2# -# x1#) (y2# -# y1#) ~=# 0.0## of 1# -> True; _ -> False

instance HasZero (Vector2D units space) where
  zero = Vector2D# 0.0## 0.0##

instance Negation (Vector2D units space) where
  negative (Vector2D# vx# vy#) = Vector2D# (negate# vx#) (negate# vy#)

instance Multiplication Sign (Vector2D units space) (Vector2D units space) where
  Positive .*. vector = vector
  Negative .*. vector = negative vector

instance Multiplication (Vector2D units space) Sign (Vector2D units space) where
  vector .*. Positive = vector
  vector .*. Negative = negative vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2D units1 space1)
    (Vector2D units2 space2)
    (Vector2D units1 space1)
  where
  Vector2D# x1# y1# .+. Vector2D# x2# y2# = Vector2D# (x1# +# x2#) (y1# +# y2#)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2D units1 space1)
    (Vector2D units2 space2)
    (Vector2D units1 space1)
  where
  Vector2D# x1# y1# .-. Vector2D# x2# y2# = Vector2D# (x1# -# x2#) (y1# -# y2#)

instance
  Multiplication_
    (Quantity units1)
    (Vector2D units2 space)
    (Vector2D (units1 ?*? units2) space)
  where
  Quantity# scale# ?*? Vector2D# vx# vy# = Vector2D# (scale# *# vx#) (scale# *# vy#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Vector2D units2 space) (Vector2D units3 space)
  where
  Quantity# scale# .*. Vector2D# vx# vy# = Vector2D# (scale# *# vx#) (scale# *# vy#)

instance
  Multiplication_
    (Vector2D units1 space)
    (Quantity units2)
    (Vector2D (units1 ?*? units2) space)
  where
  Vector2D# vx# vy# ?*? Quantity# scale# = Vector2D# (vx# *# scale#) (vy# *# scale#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2D units1 space) (Quantity units2) (Vector2D units3 space)
  where
  Vector2D# vx# vy# .*. Quantity# scale# = Vector2D# (vx# *# scale#) (vy# *# scale#)

instance
  Multiplication_
    (Bounds units1)
    (Vector2D units2 space)
    (VectorBounds2D (units1 ?*? units2) space)
  where
  bounds ?*? Vector2D vx vy = VectorBounds2D (bounds ?*? vx) (bounds ?*? vy)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Vector2D units2 space) (VectorBounds2D units3 space)
  where
  bounds .*. Vector2D vx vy = VectorBounds2D (bounds .*. vx) (bounds .*. vy)

instance
  Multiplication_
    (Vector2D units1 space)
    (Bounds units2)
    (VectorBounds2D (units1 ?*? units2) space)
  where
  Vector2D vx vy ?*? bounds = VectorBounds2D (vx ?*? bounds) (vy ?*? bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2D units1 space) (Bounds units2) (VectorBounds2D units3 space)
  where
  Vector2D vx vy .*. bounds = VectorBounds2D (vx .*. bounds) (vy .*. bounds)

instance
  Division_
    (Vector2D units1 space)
    (Quantity units2)
    (Vector2D (units1 ?/? units2) space)
  where
  Vector2D vx vy ?/? scale = Vector2D (vx ?/? scale) (vy ?/? scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector2D units1 space) (Quantity units2) (Vector2D units3 space)
  where
  Vector2D vx vy ./. scale = Vector2D (vx ./. scale) (vy ./. scale)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector2D units1 space1)
    (Vector2D units2 space2)
    (Quantity (units1 ?*? units2))
  where
  Vector2D x1 y1 `dot_` Vector2D x2 y2 = x1 ?*? x2 .+. y1 ?*? y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2D units1 space1) (Vector2D units2 space2) (Quantity units3)
  where
  Vector2D x1 y1 `dot` Vector2D x2 y2 = x1 .*. x2 .+. y1 .*. y2

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2D units space1) (Direction2D space2) (Quantity units)
  where
  v `dot` Unit2D d = v `dot` d

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2D space1) (Vector2D units space2) (Quantity units)
  where
  Unit2D d `dot` v = d `dot` v

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector2D units1 space1)
    (Vector2D units2 space2)
    (Quantity (units1 ?*? units2))
  where
  Vector2D x1 y1 `cross_` Vector2D x2 y2 = x1 ?*? y2 .-. y1 ?*? x2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication (Vector2D units1 space1) (Vector2D units2 space2) (Quantity units3)
  where
  Vector2D x1 y1 `cross` Vector2D x2 y2 = x1 .*. y2 .-. y1 .*. x2

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2D units space1) (Direction2D space2) (Quantity units)
  where
  v1 `cross` Unit2D v2 = v1 `cross` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2D space1) (Vector2D units space2) (Quantity units)
  where
  Unit2D v1 `cross` v2 = v1 `cross` v2

----- Direction2D -----

type role Direction2D phantom

{-| A direction in 2D.

This is effectively a type-safe unit vector.
-}
newtype Direction2D space = Unit2D (Vector2D Unitless space)
  deriving (Eq, Ord, Show)

{-# COMPLETE Direction2D #-}

{-# INLINE Direction2D #-}
pattern Direction2D :: Number -> Number -> Direction2D space
pattern Direction2D dX dY = Unit2D (Vector2D dX dY)

instance FFI (Direction2D FFI.Space) where
  representation = FFI.classRepresentation "Direction2D"

instance FFI (Direction2D UvSpace) where
  representation = FFI.classRepresentation "UvDirection"

instance ApproximateEquality (Direction2D space) Radians where
  d1 ~= d2 = Angle.atan2 (d1 `cross` d2) (d1 `dot` d2) ~= Angle.zero

instance Negation (Direction2D space) where
  negative (Unit2D v) = Unit2D (negative v)

instance Multiplication_ Sign (Direction2D space) (Direction2D space) where
  Positive ?*? direction = direction
  Negative ?*? direction = negative direction

instance Multiplication Sign (Direction2D space) (Direction2D space) where
  Positive .*. direction = direction
  Negative .*. direction = negative direction

instance Multiplication_ (Direction2D space) Sign (Direction2D space) where
  direction ?*? Positive = direction
  direction ?*? Negative = negative direction

instance Multiplication (Direction2D space) Sign (Direction2D space) where
  direction .*. Positive = direction
  direction .*. Negative = negative direction

instance Multiplication (Quantity units) (Direction2D space) (Vector2D units space) where
  scale .*. Unit2D v = scale .*. v

instance Multiplication (Direction2D space) (Quantity units) (Vector2D units space) where
  Unit2D v .*. scale = v .*. scale

instance space1 ~ space2 => DotMultiplication (Direction2D space1) (Direction2D space2) Number where
  Unit2D v1 `dot` Unit2D v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2D space1) (Direction2D space2) Number
  where
  Unit2D v1 `cross` Unit2D v2 = v1 `cross` v2

----- Orientation2D -----

type role Orientation2D phantom

data Orientation2D space
  = Orientation2D (Direction2D space) (Direction2D space)

instance HasField "xDirection" (Orientation2D space) (Direction2D space) where
  getField (Orientation2D dx _) = dx

instance HasField "yDirection" (Orientation2D space) (Direction2D space) where
  getField (Orientation2D _ dy) = dy

deriving instance Eq (Orientation2D space)

deriving instance Show (Orientation2D space)

----- Point2D -----

type role Point2D phantom phantom

newtype Point2D units space = Position2D (Vector2D units space)

{-# COMPLETE Point2D #-}

{-# INLINE Point2D #-}

-- | Construct a point from its X and Y coordinates.
pattern Point2D :: Quantity units -> Quantity units -> Point2D units space
pattern Point2D px py <- Position2D (Vector2D px py)
  where
    Point2D px py = Position2D (Vector2D px py)

deriving instance Eq (Point2D units space)

deriving instance Ord (Point2D units space)

deriving instance Show (Point2D units space)

instance FFI (Point2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Point2D"

instance FFI (Point2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvPoint"

instance HasUnits (Point2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Point2D unitsA space1) (Point2D unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2D units1 space1)
    (Vector2D units2 space2)
    (Point2D units1 space1)
  where
  Position2D p .+. v = Position2D (p .+. v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2D units1 space1)
    (Vector2D units2 space2)
    (Point2D units1 space1)
  where
  Position2D p .-. v = Position2D (p .-. v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2D units1 space1)
    (Point2D units2 space2)
    (Vector2D units1 space1)
  where
  Position2D p1 .-. Position2D p2 = p1 .-. p2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds2D units1 space1)
  where
  Position2D p .+. vb = PositionBounds2D (p .+. vb)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds2D units1 space1)
  where
  Position2D p .-. vb = PositionBounds2D (p .-. vb)

instance ApproximateEquality (Point2D units space) units where
  Position2D p1 ~= Position2D p2 = p1 ~= p2

----- VectorBounds2D -----

type role VectorBounds2D phantom phantom

type VectorBounds2D :: Type -> Type -> Type
data VectorBounds2D units space
  = -- | Construct a vector bounding box from its X and Y coordinate bounds.
    VectorBounds2D (Bounds units) (Bounds units)

deriving instance Show (VectorBounds2D units space)

instance HasUnits (VectorBounds2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds2D unitsA space1) (VectorBounds2D unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Vector2D units1 space1) (VectorBounds2D units2 space2) units1
  where
  Vector2D vx vy `intersects` VectorBounds2D bx by = vx `intersects` bx && vy `intersects` by

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (VectorBounds2D units1 space1) (Vector2D units2 space2) units1
  where
  box `intersects` point = point `intersects` box

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (VectorBounds2D units1 space1) (VectorBounds2D units2 space2) units1
  where
  VectorBounds2D x1 y1 `intersects` VectorBounds2D x2 y2 =
    x1 `intersects` x2 && y1 `intersects` y2

instance Negation (VectorBounds2D units space) where
  negative (VectorBounds2D x y) = VectorBounds2D (negative x) (negative y)

instance Multiplication Sign (VectorBounds2D units space) (VectorBounds2D units space) where
  Positive .*. vectorBounds = vectorBounds
  Negative .*. vectorBounds = negative vectorBounds

instance Multiplication (VectorBounds2D units space) Sign (VectorBounds2D units space) where
  vectorBounds .*. Positive = vectorBounds
  vectorBounds .*. Negative = negative vectorBounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  VectorBounds2D x1 y1 .+. VectorBounds2D x2 y2 = VectorBounds2D (x1 .+. x2) (y1 .+. y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds2D units1 space1)
    (Vector2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  VectorBounds2D x1 y1 .+. Vector2D x2 y2 = VectorBounds2D (x1 .+. x2) (y1 .+. y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector2D units1 space1)
    (VectorBounds2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  Vector2D x1 y1 .+. VectorBounds2D x2 y2 = VectorBounds2D (x1 .+. x2) (y1 .+. y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  VectorBounds2D x1 y1 .-. VectorBounds2D x2 y2 = VectorBounds2D (x1 .-. x2) (y1 .-. y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds2D units1 space1)
    (Vector2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  VectorBounds2D x1 y1 .-. Vector2D x2 y2 = VectorBounds2D (x1 .-. x2) (y1 .-. y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector2D units1 space1)
    (VectorBounds2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  Vector2D x1 y1 .-. VectorBounds2D x2 y2 = VectorBounds2D (x1 .-. x2) (y1 .-. y2)

instance
  Multiplication_
    (Quantity units1)
    (VectorBounds2D units2 space)
    (VectorBounds2D (units1 ?*? units2) space)
  where
  value ?*? VectorBounds2D x y = VectorBounds2D (value ?*? x) (value ?*? y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorBounds2D units2 space)
    (VectorBounds2D units3 space)
  where
  value .*. VectorBounds2D x y = VectorBounds2D (value .*. x) (value .*. y)

instance
  Multiplication_
    (VectorBounds2D units1 space)
    (Quantity units2)
    (VectorBounds2D (units1 ?*? units2) space)
  where
  VectorBounds2D x y ?*? value = VectorBounds2D (x ?*? value) (y ?*? value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds2D units1 space)
    (Quantity units2)
    (VectorBounds2D units3 space)
  where
  VectorBounds2D x y .*. value = VectorBounds2D (x .*. value) (y .*. value)

instance
  Multiplication_
    (Bounds units1)
    (VectorBounds2D units2 space)
    (VectorBounds2D (units1 ?*? units2) space)
  where
  bounds ?*? VectorBounds2D x y = VectorBounds2D (bounds ?*? x) (bounds ?*? y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Bounds units1)
    (VectorBounds2D units2 space)
    (VectorBounds2D units3 space)
  where
  bounds .*. VectorBounds2D x y = VectorBounds2D (bounds .*. x) (bounds .*. y)

instance
  Multiplication_
    (VectorBounds2D units1 space)
    (Bounds units2)
    (VectorBounds2D (units1 ?*? units2) space)
  where
  VectorBounds2D x y ?*? bounds = VectorBounds2D (x ?*? bounds) (y ?*? bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds2D units1 space)
    (Bounds units2)
    (VectorBounds2D units3 space)
  where
  VectorBounds2D x y .*. bounds = VectorBounds2D (x .*. bounds) (y .*. bounds)

instance
  Division_
    (VectorBounds2D units1 space)
    (Quantity units2)
    (VectorBounds2D (units1 ?/? units2) space)
  where
  VectorBounds2D x y ?/? value = VectorBounds2D (x ?/? value) (y ?/? value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2D units1 space) (Quantity units2) (VectorBounds2D units3 space)
  where
  VectorBounds2D x y ./. value = VectorBounds2D (x ./. value) (y ./. value)

instance
  Division_
    (VectorBounds2D units1 space)
    (Bounds units2)
    (VectorBounds2D (units1 ?/? units2) space)
  where
  VectorBounds2D x y ?/? bounds = VectorBounds2D (x ?/? bounds) (y ?/? bounds)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2D units1 space) (Bounds units2) (VectorBounds2D units3 space)
  where
  VectorBounds2D x y ./. bounds = VectorBounds2D (x ./. bounds) (y ./. bounds)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2D units1 space1) (VectorBounds2D units2 space2) (Bounds units3)
  where
  Vector2D x1 y1 `dot` VectorBounds2D x2 y2 = x1 .*. x2 .+. y1 .*. y2

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  Vector2D x1 y1 `dot_` VectorBounds2D x2 y2 = x1 ?*? x2 .+. y1 ?*? y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorBounds2D units1 space1) (Vector2D units2 space2) (Bounds units3)
  where
  VectorBounds2D x1 y1 `dot` Vector2D x2 y2 = x1 .*. x2 .+. y1 .*. y2

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorBounds2D units1 space1)
    (Vector2D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  VectorBounds2D x1 y1 `dot_` Vector2D x2 y2 = x1 ?*? x2 .+. y1 ?*? y2

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2D space1) (VectorBounds2D units space2) (Bounds units)
  where
  Unit2D vector `dot` vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds2D units space1) (Direction2D space2) (Bounds units)
  where
  vectorBounds `dot` Unit2D vector = vectorBounds `dot` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorBounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds units3)
  where
  VectorBounds2D x1 y1 `dot` VectorBounds2D x2 y2 = x1 .*. x2 .+. y1 .*. y2

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorBounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  VectorBounds2D x1 y1 `dot_` VectorBounds2D x2 y2 = x1 ?*? x2 .+. y1 ?*? y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds units3)
  where
  Vector2D x1 y1 `cross` VectorBounds2D x2 y2 = x1 .*. y2 .-. y1 .*. x2

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  Vector2D x1 y1 `cross_` VectorBounds2D x2 y2 = x1 ?*? y2 .-. y1 ?*? x2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds2D units1 space1)
    (Vector2D units2 space2)
    (Bounds units3)
  where
  VectorBounds2D x1 y1 `cross` Vector2D x2 y2 = x1 .*. y2 .-. y1 .*. x2

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorBounds2D units1 space1)
    (Vector2D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  VectorBounds2D x1 y1 `cross_` Vector2D x2 y2 = x1 ?*? y2 .-. y1 ?*? x2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2D space1) (VectorBounds2D units space2) (Bounds units)
  where
  Unit2D vector `cross` vectorBounds = vector `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorBounds2D units space1) (Direction2D space2) (Bounds units)
  where
  vectorBounds `cross` Unit2D vector = vectorBounds `cross` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds units3)
  where
  VectorBounds2D x1 y1 `cross` VectorBounds2D x2 y2 = x1 .*. y2 .-. y1 .*. x2

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorBounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  VectorBounds2D x1 y1 `cross_` VectorBounds2D x2 y2 = x1 ?*? y2 .-. y1 ?*? x2

----- Bounds2D -----

type role Bounds2D phantom phantom

newtype Bounds2D units space
  = PositionBounds2D (VectorBounds2D units space)

{-# COMPLETE Bounds2D #-}

{-# INLINE Bounds2D #-}

-- | Construct a bounding box from its X and Y coordinate bounds.
pattern Bounds2D :: Bounds units -> Bounds units -> Bounds2D units space
pattern Bounds2D bx by <- PositionBounds2D (VectorBounds2D bx by)
  where
    Bounds2D bx by = PositionBounds2D (VectorBounds2D bx by)

deriving instance Show (Bounds2D units space)

instance HasUnits (Bounds2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds2D unitsA space1) (Bounds2D unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance FFI (Bounds2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Bounds2D"

instance FFI (Bounds2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvBounds"

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2D units1 space1)
    (Bounds2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  Position2D p .-. PositionBounds2D pb = p .-. pb

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2D units1 space1)
    (Point2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  PositionBounds2D pb .-. Position2D p = pb .-. p

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2D units1 space1)
    (Bounds2D units2 space2)
    (VectorBounds2D units1 space1)
  where
  PositionBounds2D pb1 .-. PositionBounds2D pb2 = pb1 .-. pb2

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Bounds2D units1 space1)
    (Vector2D units2 space2)
    (Bounds2D units1 space1)
  where
  PositionBounds2D pb .+. v = PositionBounds2D (pb .+. v)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Bounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds2D units1 space1)
  where
  PositionBounds2D pb .+. vb = PositionBounds2D (pb .+. vb)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2D units1 space1)
    (Vector2D units2 space2)
    (Bounds2D units1 space1)
  where
  PositionBounds2D pb .-. v = PositionBounds2D (pb .-. v)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2D units1 space1)
    (VectorBounds2D units2 space2)
    (Bounds2D units1 space1)
  where
  PositionBounds2D pb .-. vb = PositionBounds2D (pb .-. vb)

instance
  (units1 ~ units2, space1 ~ space2) =>
  Intersects (Point2D units1 space1) (Axis2D units2 space2) units1
  where
  p `intersects` (Axis2D p0 d) = (p .-. p0) `cross` d ~= Quantity.zero

instance
  (units1 ~ units2, space1 ~ space2) =>
  Intersects (Axis2D units2 space2) (Point2D units1 space1) units1
  where
  axis `intersects` point = point `intersects` axis

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2D units1 space1) (Bounds2D units2 space2) units1
  where
  Position2D p `intersects` PositionBounds2D pb = p `intersects` pb

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Bounds2D units1 space1) (Point2D units2 space2) units1
  where
  box `intersects` point = point `intersects` box

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Bounds2D units1 space1) (Bounds2D units2 space2) units1
  where
  PositionBounds2D pb1 `intersects` PositionBounds2D pb2 = pb1 `intersects` pb2

----- Axis2D -----

type role Axis2D phantom phantom

-- | An axis in 2D, defined by an origin point and direction.
data Axis2D units space
  = -- | Construct an axis from its origin point and direction.
    Axis2D
    { originPoint :: Point2D units space
    , direction :: Direction2D space
    }

deriving instance Eq (Axis2D units space)

deriving instance Show (Axis2D units space)

instance HasUnits (Axis2D units space) units

instance FFI (Axis2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Axis2D"

instance FFI (Axis2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvAxis"

----- Frame2D -----

type role Frame2D phantom phantom phantom

type Frame2D :: Type -> Type -> Type -> Type
data Frame2D units global local
  = Frame2D
  { originPoint :: Point2D units global
  , orientation :: Orientation2D global
  }

instance HasField "xDirection" (Frame2D units global local) (Direction2D global) where
  getField = (.orientation.xDirection)

instance HasField "yDirection" (Frame2D units global local) (Direction2D global) where
  getField = (.orientation.yDirection)

deriving instance Eq (Frame2D units global local)

deriving instance Show (Frame2D units global local)

instance FFI (Frame2D Meters FFI.Space local) where
  representation = FFI.classRepresentation "Frame2D"

instance FFI (Frame2D Unitless UvSpace local) where
  representation = FFI.classRepresentation "UvFrame"

----- Transform2D -----

type role Transform2D phantom phantom phantom

type Transform2D :: Type -> Type -> Type -> Type
data Transform2D tag units space
  = Transform2D (Point2D units space) (Vector2D Unitless space) (Vector2D Unitless space)

deriving instance Eq (Transform2D tag units space)

deriving instance Ord (Transform2D tag units space)

deriving instance Show (Transform2D tag units space)

instance HasUnits (Transform2D tag units space) units

instance
  (tag1 ~ tag2, space1 ~ space2) =>
  Units.Coercion
    (Transform2D tag1 unitsA space1)
    (Transform2D tag2 unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance
  space1 ~ space2 =>
  Multiplication
    (Transform2D tag translationUnits space1)
    (Vector2D units space2)
    (Vector2D units space1)
  where
  transform .*. vector = vector .*. transform

instance
  space1 ~ space2 =>
  Multiplication
    (Vector2D units space1)
    (Transform2D tag translationUnits space2)
    (Vector2D units space1)
  where
  Vector2D vx vy .*. Transform2D _ i j = vx .*. i .+. vy .*. j

instance
  (space1 ~ space2, units1 ~ units2) =>
  Multiplication
    (Point2D units1 space1)
    (Transform2D tag units2 space2)
    (Point2D units1 space1)
  where
  Point2D px py .*. Transform2D p0 i j = p0 .+. px .*. i .+. py .*. j

instance
  (space1 ~ space2, units1 ~ units2) =>
  Multiplication
    (Transform2D tag units1 space1)
    (Point2D units2 space2)
    (Point2D units1 space1)
  where
  transform .*. point = point .*. transform

instance
  (Composition tag1 tag2 tag3, space1 ~ space2, units1 ~ units2) =>
  Composition
    (Transform2D tag1 units1 space1)
    (Transform2D tag2 units2 space2)
    (Transform2D tag3 units1 space1)
  where
  transform2 `compose` transform1 =
    Transform2D
      (Point2D Quantity.zero Quantity.zero .*. transform1 .*. transform2)
      (Vector2D 1 0 .*. transform1 .*. transform2)
      (Vector2D 0 1 .*. transform1 .*. transform2)

----- Vector3D -----

type role Vector3D phantom phantom

type Vector3D :: Type -> Type -> Type
data Vector3D units space = Vector3D# Double# Double# Double#
  deriving (Eq, Ord, Show)

-- | Construct a vector from its X and Y components.
{-# INLINE Vector3D #-}
pattern Vector3D :: Quantity units -> Quantity units -> Quantity units -> Vector3D units space
pattern Vector3D vx vy vz <- (viewVector3D -> (# vx, vy, vz #))
  where
    Vector3D (Quantity# vx#) (Quantity# vy#) (Quantity# vz#) = Vector3D# vx# vy# vz#

{-# INLINE viewVector3D #-}
viewVector3D :: Vector3D units space -> (# Quantity units, Quantity units, Quantity units #)
viewVector3D (Vector3D# vx# vy# vz#) = (# Quantity# vx#, Quantity# vy#, Quantity# vz# #)

{-# COMPLETE Vector3D #-}

instance FFI (Vector3D Unitless FFI.Space) where
  representation = FFI.classRepresentation "Vector3D"

instance FFI (Vector3D Meters FFI.Space) where
  representation = FFI.classRepresentation "Displacement3D"

instance FFI (Vector3D SquareMeters FFI.Space) where
  representation = FFI.classRepresentation "AreaVector3D"

instance HasUnits (Vector3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector3D unitsA space1)
    (Vector3D unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance ApproximateEquality (Vector3D units space) units where
  Vector3D x1 y1 z1 ~= Vector3D x2 y2 z2 =
    Quantity.hypot3 (x2 .-. x1) (y2 .-. y1) (z2 .-. z1) ~= Quantity.zero

instance HasZero (Vector3D units space) where
  zero = Vector3D Quantity.zero Quantity.zero Quantity.zero

instance Negation (Vector3D units space) where
  negative (Vector3D vx vy vz) = Vector3D (negative vx) (negative vy) (negative vz)

instance Multiplication Sign (Vector3D units space) (Vector3D units space) where
  Positive .*. vector = vector
  Negative .*. vector = negative vector

instance Multiplication (Vector3D units space) Sign (Vector3D units space) where
  vector .*. Positive = vector
  vector .*. Negative = negative vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3D units1 space1)
    (Vector3D units2 space2)
    (Vector3D units1 space1)
  where
  Vector3D x1 y1 z1 .+. Vector3D x2 y2 z2 = Vector3D (x1 .+. x2) (y1 .+. y2) (z1 .+. z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3D units1 space1)
    (Vector3D units2 space2)
    (Vector3D units1 space1)
  where
  Vector3D x1 y1 z1 .-. Vector3D x2 y2 z2 = Vector3D (x1 .-. x2) (y1 .-. y2) (z1 .-. z2)

instance
  Multiplication_
    (Quantity units1)
    (Vector3D units2 space)
    (Vector3D (units1 ?*? units2) space)
  where
  scale ?*? Vector3D vx vy vz = Vector3D (scale ?*? vx) (scale ?*? vy) (scale ?*? vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Vector3D units2 space) (Vector3D units3 space)
  where
  scale .*. Vector3D vx vy vz = Vector3D (scale .*. vx) (scale .*. vy) (scale .*. vz)

instance
  Multiplication_
    (Vector3D units1 space)
    (Quantity units2)
    (Vector3D (units1 ?*? units2) space)
  where
  Vector3D vx vy vz ?*? scale = Vector3D (vx ?*? scale) (vy ?*? scale) (vz ?*? scale)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3D units1 space) (Quantity units2) (Vector3D units3 space)
  where
  Vector3D vx vy vz .*. scale = Vector3D (vx .*. scale) (vy .*. scale) (vz .*. scale)

instance
  Multiplication_
    (Bounds units1)
    (Vector3D units2 space)
    (VectorBounds3D (units1 ?*? units2) space)
  where
  bounds ?*? Vector3D vx vy vz = VectorBounds3D (bounds ?*? vx) (bounds ?*? vy) (bounds ?*? vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Vector3D units2 space) (VectorBounds3D units3 space)
  where
  bounds .*. Vector3D vx vy vz = VectorBounds3D (bounds .*. vx) (bounds .*. vy) (bounds .*. vz)

instance
  Multiplication_
    (Vector3D units1 space)
    (Bounds units2)
    (VectorBounds3D (units1 ?*? units2) space)
  where
  Vector3D vx vy vz ?*? bounds = VectorBounds3D (vx ?*? bounds) (vy ?*? bounds) (vz ?*? bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3D units1 space) (Bounds units2) (VectorBounds3D units3 space)
  where
  Vector3D vx vy vz .*. bounds = VectorBounds3D (vx .*. bounds) (vy .*. bounds) (vz .*. bounds)

instance
  Division_
    (Vector3D units1 space)
    (Quantity units2)
    (Vector3D (units1 ?/? units2) space)
  where
  Vector3D vx vy vz ?/? scale = Vector3D (vx ?/? scale) (vy ?/? scale) (vz ?/? scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector3D units1 space) (Quantity units2) (Vector3D units3 space)
  where
  Vector3D vx vy vz ./. scale = Vector3D (vx ./. scale) (vy ./. scale) (vz ./. scale)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector3D units1 space1)
    (Vector3D units2 space2)
    (Quantity (units1 ?*? units2))
  where
  Vector3D x1 y1 z1 `dot_` Vector3D x2 y2 z2 = x1 ?*? x2 .+. y1 ?*? y2 .+. z1 ?*? z2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3D units1 space1) (Vector3D units2 space2) (Quantity units3)
  where
  Vector3D x1 y1 z1 `dot` Vector3D x2 y2 z2 = x1 .*. x2 .+. y1 .*. y2 .+. z1 .*. z2

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3D units space1) (Direction3D space2) (Quantity units)
  where
  v1 `dot` Unit3D v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3D space1) (Vector3D units space2) (Quantity units)
  where
  Unit3D v1 `dot` v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector3D units1 space1)
    (Vector3D units2 space2)
    (Vector3D (units1 ?*? units2) space1)
  where
  Vector3D x1 y1 z1 `cross_` Vector3D x2 y2 z2 =
    Vector3D
      (y1 ?*? z2 .-. z1 ?*? y2)
      (z1 ?*? x2 .-. x1 ?*? z2)
      (x1 ?*? y2 .-. y1 ?*? x2)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3D units1 space1)
    (Vector3D units2 space2)
    (Vector3D units3 space1)
  where
  Vector3D x1 y1 z1 `cross` Vector3D x2 y2 z2 =
    Vector3D
      (y1 .*. z2 .-. z1 .*. y2)
      (z1 .*. x2 .-. x1 .*. z2)
      (x1 .*. y2 .-. y1 .*. x2)

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector3D units space1) (Direction3D space2) (Vector3D units space1)
  where
  v1 `cross` Unit3D v2 = v1 `cross` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction3D space1) (Vector3D units space2) (Vector3D units space1)
  where
  Unit3D v1 `cross` v2 = v1 `cross` v2

----- Direction3D -----

type role Direction3D phantom

{-| A direction in 3D.

This is effectively a type-safe unit vector.
-}
newtype Direction3D space = Unit3D (Vector3D Unitless space)
  deriving (Eq, Ord, Show)

{-# COMPLETE Direction3D #-}

{-# INLINE Direction3D #-}
pattern Direction3D :: Number -> Number -> Number -> Direction3D space
pattern Direction3D dR dF dU = Unit3D (Vector3D dR dF dU)

instance FFI (Direction3D FFI.Space) where
  representation = FFI.classRepresentation "Direction3D"

instance ApproximateEquality (Direction3D space) Radians where
  d1 ~= d2 = do
    let parallel = d1 `dot` d2
    let Vector3D cx cy cz = d1 `cross` d2
    let perpendicular = Quantity.hypot3 cx cy cz
    Angle.atan2 perpendicular parallel ~= Quantity.zero

instance Negation (Direction3D space) where
  negative (Unit3D vector) = Unit3D (negative vector)

instance Multiplication Sign (Direction3D space) (Direction3D space) where
  Positive .*. direction = direction
  Negative .*. direction = negative direction

instance Multiplication_ Sign (Direction3D space) (Direction3D space) where
  Positive ?*? direction = direction
  Negative ?*? direction = negative direction

instance Multiplication (Direction3D space) Sign (Direction3D space) where
  direction .*. Positive = direction
  direction .*. Negative = negative direction

instance Multiplication_ (Direction3D space) Sign (Direction3D space) where
  direction ?*? Positive = direction
  direction ?*? Negative = negative direction

instance Multiplication (Quantity units) (Direction3D space) (Vector3D units space) where
  scale .*. Unit3D vector = scale .*. vector

instance Multiplication (Direction3D space) (Quantity units) (Vector3D units space) where
  Unit3D vector .*. scale = vector .*. scale

instance Multiplication (Bounds units) (Direction3D space) (VectorBounds3D units space) where
  bounds .*. Unit3D vector = bounds .*. vector

instance Multiplication (Direction3D space) (Bounds units) (VectorBounds3D units space) where
  Unit3D vector .*. bounds = vector .*. bounds

instance space1 ~ space2 => DotMultiplication (Direction3D space1) (Direction3D space2) Number where
  Unit3D vector1 `dot` Unit3D vector2 = vector1 `dot` vector2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction3D space1) (Direction3D space2) (Vector3D Unitless space1)
  where
  Unit3D vector1 `cross` Unit3D vector2 = vector1 `cross` vector2

----- PlaneOrientation3D -----

type role PlaneOrientation3D phantom

-- | A pair of perpendicular X and Y directions defining the orientation of a plane in 3D.
data PlaneOrientation3D space = PlaneOrientation3D (Direction3D space) (Direction3D space)

deriving instance Eq (PlaneOrientation3D space)

deriving instance Ord (PlaneOrientation3D space)

deriving instance Show (PlaneOrientation3D space)

instance FFI (PlaneOrientation3D FFI.Space) where
  representation = FFI.classRepresentation "PlaneOrientation3D"

instance HasField "xDirection" (PlaneOrientation3D space) (Direction3D space) where
  getField (PlaneOrientation3D dx _) = dx

instance HasField "yDirection" (PlaneOrientation3D space) (Direction3D space) where
  getField (PlaneOrientation3D _ dy) = dy

instance HasField "normalDirection" (PlaneOrientation3D space) (Direction3D space) where
  getField (PlaneOrientation3D dx dy) = Unit3D (dx `cross` dy)

----- Orientation3D -----

type role Orientation3D phantom

-- | A set of cardinal directions (forward, upward etc.) defining a 3D orientation.
data Orientation3D space
  = Orientation3D (Direction3D space) (Direction3D space) (Direction3D space)

deriving instance Eq (Orientation3D space)

deriving instance Show (Orientation3D space)

instance FFI (Orientation3D FFI.Space) where
  representation = FFI.classRepresentation "Orientation3D"

instance HasField "rightwardDirection" (Orientation3D space) (Direction3D space) where
  getField (Orientation3D r _ _) = r

instance HasField "leftwardDirection" (Orientation3D space) (Direction3D space) where
  getField orientation = negative orientation.rightwardDirection

instance HasField "forwardDirection" (Orientation3D space) (Direction3D space) where
  getField (Orientation3D _ f _) = f

instance HasField "backwardDirection" (Orientation3D space) (Direction3D space) where
  getField orientation = negative orientation.forwardDirection

instance HasField "upwardDirection" (Orientation3D space) (Direction3D space) where
  getField (Orientation3D _ _ u) = u

instance HasField "downwardDirection" (Orientation3D space) (Direction3D space) where
  getField orientation = negative orientation.upwardDirection

instance HasField "rightPlaneOrientation" (Orientation3D space) (PlaneOrientation3D space) where
  getField orientation =
    PlaneOrientation3D orientation.forwardDirection orientation.upwardDirection

instance HasField "leftPlaneOrientation" (Orientation3D space) (PlaneOrientation3D space) where
  getField orientation =
    PlaneOrientation3D orientation.backwardDirection orientation.upwardDirection

instance HasField "frontPlaneOrientation" (Orientation3D space) (PlaneOrientation3D space) where
  getField orientation =
    PlaneOrientation3D orientation.leftwardDirection orientation.upwardDirection

instance HasField "backPlaneOrientation" (Orientation3D space) (PlaneOrientation3D space) where
  getField orientation =
    PlaneOrientation3D orientation.rightwardDirection orientation.upwardDirection

instance HasField "topPlaneOrientation" (Orientation3D space) (PlaneOrientation3D space) where
  getField orientation =
    PlaneOrientation3D orientation.rightwardDirection orientation.forwardDirection

instance HasField "bottomPlaneOrientation" (Orientation3D space) (PlaneOrientation3D space) where
  getField orientation =
    PlaneOrientation3D orientation.leftwardDirection orientation.forwardDirection

instance HasField "backwardOrientation" (Orientation3D space) (Orientation3D space) where
  getField orientation =
    Orientation3D
      orientation.leftwardDirection
      orientation.backwardDirection
      orientation.upwardDirection

instance HasField "rightwardOrientation" (Orientation3D space) (Orientation3D space) where
  getField orientation =
    Orientation3D
      orientation.backwardDirection
      orientation.rightwardDirection
      orientation.upwardDirection

instance HasField "leftwardOrientation" (Orientation3D space) (Orientation3D space) where
  getField orientation =
    Orientation3D
      orientation.forwardDirection
      orientation.leftwardDirection
      orientation.upwardDirection

instance HasField "upwardOrientation" (Orientation3D space) (Orientation3D space) where
  getField orientation =
    Orientation3D
      orientation.leftwardDirection
      orientation.upwardDirection
      orientation.forwardDirection

instance HasField "downwardOrientation" (Orientation3D space) (Orientation3D space) where
  getField orientation =
    Orientation3D
      orientation.rightwardDirection
      orientation.downwardDirection
      orientation.forwardDirection

----- Point3D -----

type role Point3D phantom

newtype Point3D space = Position3D (Vector3D Meters space)

{-# COMPLETE Point3D #-}

{-# INLINE Point3D #-}

-- | Construct a point from its X and Y coordinates.
pattern Point3D :: Length -> Length -> Length -> Point3D space
pattern Point3D px py pz <- Position3D (Vector3D px py pz)
  where
    Point3D px py pz = Position3D (Vector3D px py pz)

deriving instance Eq (Point3D space)

deriving instance Ord (Point3D space)

deriving instance Show (Point3D space)

instance FFI (Point3D FFI.Space) where
  representation = FFI.classRepresentation "Point3D"

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Addition
    (Point3D space1)
    (Vector3D meters space2)
    (Point3D space1)
  where
  Position3D p .+. v = Position3D (p .+. v)

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Subtraction
    (Point3D space1)
    (Vector3D meters space2)
    (Point3D space1)
  where
  Position3D p .-. v = Position3D (p .-. v)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3D space1)
    (Point3D space2)
    (Vector3D Meters space1)
  where
  Position3D p1 .-. Position3D p2 = p1 .-. p2

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Addition
    (Point3D space1)
    (VectorBounds3D meters space2)
    (Bounds3D space1)
  where
  Position3D p .+. vb = PositionBounds3D (p .+. vb)

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Subtraction
    (Point3D space1)
    (VectorBounds3D meters space2)
    (Bounds3D space1)
  where
  Position3D p .-. vb = PositionBounds3D (p .-. vb)

instance ApproximateEquality (Point3D space) Meters where
  Position3D p1 ~= Position3D p2 = p1 ~= p2

----- VectorBounds3D -----

type role VectorBounds3D phantom phantom

type VectorBounds3D :: Type -> Type -> Type
data VectorBounds3D units space
  = VectorBounds3D# Double# Double# Double# Double# Double# Double#
  deriving (Show)

-- | Construct a vector bounds from its rightward, forward and upward components.
{-# INLINE VectorBounds3D #-}
pattern VectorBounds3D ::
  Bounds units ->
  Bounds units ->
  Bounds units ->
  VectorBounds3D units space
pattern VectorBounds3D x y z <- (viewVectorBounds3D -> (# x, y, z #))
  where
    VectorBounds3D (Bounds# xl# xh#) (Bounds# yl# yh#) (Bounds# zl# zh#) =
      VectorBounds3D# xl# xh# yl# yh# zl# zh#

viewVectorBounds3D ::
  VectorBounds3D units space ->
  (# Bounds units, Bounds units, Bounds units #)
viewVectorBounds3D (VectorBounds3D# xl# xh# yl# yh# zl# zh#) =
  (# Bounds# xl# xh#, Bounds# yl# yh#, Bounds# zl# zh# #)

{-# COMPLETE VectorBounds3D #-}

instance HasUnits (VectorBounds3D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds3D unitsA space1) (VectorBounds3D unitsB space2)
  where
  coerce = Data.Coerce.coerce

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Vector3D units1 space1) (VectorBounds3D units2 space2) units1
  where
  Vector3D vR vF vU `intersects` VectorBounds3D bR bF bU =
    vR `intersects` bR && vF `intersects` bF && vU `intersects` bU

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (VectorBounds3D units1 space1) (Vector3D units2 space2) units1
  where
  box `intersects` point = point `intersects` box

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (VectorBounds3D units1 space1) (VectorBounds3D units2 space2) units1
  where
  VectorBounds3D r1 f1 u1 `intersects` VectorBounds3D r2 f2 u2 =
    r1 `intersects` r2 && f1 `intersects` f2 && u1 `intersects` u2

instance Negation (VectorBounds3D units space) where
  negative (VectorBounds3D# xl# xh# yl# yh# zl# zh#) = do
    VectorBounds3D#
      (negate# xh#)
      (negate# xl#)
      (negate# yh#)
      (negate# yl#)
      (negate# zh#)
      (negate# zl#)

instance
  Multiplication
    Sign
    (VectorBounds3D units space)
    (VectorBounds3D units space)
  where
  Positive .*. vectorBounds = vectorBounds
  Negative .*. vectorBounds = negative vectorBounds

instance
  Multiplication
    (VectorBounds3D units space)
    Sign
    (VectorBounds3D units space)
  where
  vectorBounds .*. Positive = vectorBounds
  vectorBounds .*. Negative = negative vectorBounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D units1 space1)
  where
  VectorBounds3D# xl1# xh1# yl1# yh1# zl1# zh1#
    .+. VectorBounds3D# xl2# xh2# yl2# yh2# zl2# zh2# = do
      let !(# xl#, xh# #) = boundsPlusBounds# xl1# xh1# xl2# xh2#
      let !(# yl#, yh# #) = boundsPlusBounds# yl1# yh1# yl2# yh2#
      let !(# zl#, zh# #) = boundsPlusBounds# zl1# zh1# zl2# zh2#
      VectorBounds3D# xl# xh# yl# yh# zl# zh#

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorBounds3D units1 space1)
    (Vector3D units2 space2)
    (VectorBounds3D units1 space1)
  where
  VectorBounds3D x1 y1 z1 .+. Vector3D x2 y2 z2 = VectorBounds3D (x1 .+. x2) (y1 .+. y2) (z1 .+. z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Vector3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D units1 space1)
  where
  Vector3D x1 y1 z1 .+. VectorBounds3D x2 y2 z2 = VectorBounds3D (x1 .+. x2) (y1 .+. y2) (z1 .+. z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D units1 space1)
  where
  VectorBounds3D# xl1# xh1# yl1# yh1# zl1# zh1#
    .-. VectorBounds3D# xl2# xh2# yl2# yh2# zl2# zh2# = do
      let !(# xl#, xh# #) = boundsMinusBounds# xl1# xh1# xl2# xh2#
      let !(# yl#, yh# #) = boundsMinusBounds# yl1# yh1# yl2# yh2#
      let !(# zl#, zh# #) = boundsMinusBounds# zl1# zh1# zl2# zh2#
      VectorBounds3D# xl# xh# yl# yh# zl# zh#

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorBounds3D units1 space1)
    (Vector3D units2 space2)
    (VectorBounds3D units1 space1)
  where
  VectorBounds3D x1 y1 z1 .-. Vector3D x2 y2 z2 = VectorBounds3D (x1 .-. x2) (y1 .-. y2) (z1 .-. z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Vector3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D units1 space1)
  where
  Vector3D x1 y1 z1 .-. VectorBounds3D x2 y2 z2 = VectorBounds3D (x1 .-. x2) (y1 .-. y2) (z1 .-. z2)

{-# INLINE quantityTimesVectorBounds3D #-}
quantityTimesVectorBounds3D ::
  Quantity units1 ->
  VectorBounds3D units2 space ->
  VectorBounds3D units3 space
quantityTimesVectorBounds3D
  (Quantity# v1#)
  (VectorBounds3D# xl2# xh2# yl2# yh2# zl2# zh2#) = do
    let !(# xl#, xh# #) = doubleTimesBounds# v1# xl2# xh2#
    let !(# yl#, yh# #) = doubleTimesBounds# v1# yl2# yh2#
    let !(# zl#, zh# #) = doubleTimesBounds# v1# zl2# zh2#
    VectorBounds3D# xl# xh# yl# yh# zl# zh#

instance
  Multiplication_
    (Quantity units1)
    (VectorBounds3D units2 space)
    (VectorBounds3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs = quantityTimesVectorBounds3D lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorBounds3D units2 space)
    (VectorBounds3D units3 space)
  where
  lhs .*. rhs = quantityTimesVectorBounds3D lhs rhs

instance
  Multiplication_
    (VectorBounds3D units1 space)
    (Quantity units2)
    (VectorBounds3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs = quantityTimesVectorBounds3D rhs lhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds3D units1 space)
    (Quantity units2)
    (VectorBounds3D units3 space)
  where
  lhs .*. rhs = quantityTimesVectorBounds3D rhs lhs

{-# INLINE boundsTimesVectorBounds3D #-}
boundsTimesVectorBounds3D ::
  Bounds units1 ->
  VectorBounds3D units2 space ->
  VectorBounds3D units3 space
boundsTimesVectorBounds3D
  (Bounds# vl1# vh1#)
  (VectorBounds3D# xl2# xh2# yl2# yh2# zl2# zh2#) = do
    let !(# xl#, xh# #) = boundsTimesBounds# vl1# vh1# xl2# xh2#
    let !(# yl#, yh# #) = boundsTimesBounds# vl1# vh1# yl2# yh2#
    let !(# zl#, zh# #) = boundsTimesBounds# vl1# vh1# zl2# zh2#
    VectorBounds3D# xl# xh# yl# yh# zl# zh#

instance
  Multiplication_
    (Bounds units1)
    (VectorBounds3D units2 space)
    (VectorBounds3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs = boundsTimesVectorBounds3D lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Bounds units1)
    (VectorBounds3D units2 space)
    (VectorBounds3D units3 space)
  where
  lhs .*. rhs = boundsTimesVectorBounds3D lhs rhs

instance
  Multiplication_
    (VectorBounds3D units1 space)
    (Bounds units2)
    (VectorBounds3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs = boundsTimesVectorBounds3D rhs lhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds3D units1 space)
    (Bounds units2)
    (VectorBounds3D units3 space)
  where
  lhs .*. rhs = boundsTimesVectorBounds3D rhs lhs

instance
  Division_
    (VectorBounds3D units1 space)
    (Quantity units2)
    (VectorBounds3D (units1 ?/? units2) space)
  where
  VectorBounds3D x y z ?/? value = VectorBounds3D (x ?/? value) (y ?/? value) (z ?/? value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3D units1 space) (Quantity units2) (VectorBounds3D units3 space)
  where
  VectorBounds3D x y z ./. value = VectorBounds3D (x ./. value) (y ./. value) (z ./. value)

instance
  Division_
    (VectorBounds3D units1 space)
    (Bounds units2)
    (VectorBounds3D (units1 ?/? units2) space)
  where
  VectorBounds3D x y z ?/? bounds = VectorBounds3D (x ?/? bounds) (y ?/? bounds) (z ?/? bounds)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3D units1 space) (Bounds units2) (VectorBounds3D units3 space)
  where
  VectorBounds3D x y z ./. bounds = VectorBounds3D (x ./. bounds) (y ./. bounds) (z ./. bounds)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3D units1 space1) (VectorBounds3D units2 space2) (Bounds units3)
  where
  Vector3D x1 y1 z1 `dot` VectorBounds3D x2 y2 z2 = x1 .*. x2 .+. y1 .*. y2 .+. z1 .*. z2

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector3D units1 space1)
    (VectorBounds3D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  Vector3D x1 y1 z1 `dot_` VectorBounds3D x2 y2 z2 = x1 ?*? x2 .+. y1 ?*? y2 .+. z1 ?*? z2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorBounds3D units1 space1) (Vector3D units2 space2) (Bounds units3)
  where
  VectorBounds3D x1 y1 z1 `dot` Vector3D x2 y2 z2 = x1 .*. x2 .+. y1 .*. y2 .+. z1 .*. z2

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorBounds3D units1 space1)
    (Vector3D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  VectorBounds3D x1 y1 z1 `dot_` Vector3D x2 y2 z2 = x1 ?*? x2 .+. y1 ?*? y2 .+. z1 ?*? z2

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3D space1) (VectorBounds3D units space2) (Bounds units)
  where
  Unit3D vector `dot` vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds3D units space1) (Direction3D space2) (Bounds units)
  where
  vectorBounds `dot` Unit3D vector = vectorBounds `dot` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorBounds3D units1 space1)
    (VectorBounds3D units2 space2)
    (Bounds units3)
  where
  VectorBounds3D x1 y1 z1 `dot` VectorBounds3D x2 y2 z2 = x1 .*. x2 .+. y1 .*. y2 .+. z1 .*. z2

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorBounds3D units1 space1)
    (VectorBounds3D units2 space2)
    (Bounds (units1 ?*? units2))
  where
  VectorBounds3D x1 y1 z1 `dot_` VectorBounds3D x2 y2 z2 = x1 ?*? x2 .+. y1 ?*? y2 .+. z1 ?*? z2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D units3 space1)
  where
  Vector3D x1 y1 z1 `cross` VectorBounds3D x2 y2 z2 =
    VectorBounds3D
      (y1 .*. z2 .-. z1 .*. y2)
      (z1 .*. x2 .-. x1 .*. z2)
      (x1 .*. y2 .-. y1 .*. x2)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D (units1 ?*? units2) space1)
  where
  Vector3D x1 y1 z1 `cross_` VectorBounds3D x2 y2 z2 =
    VectorBounds3D
      (y1 ?*? z2 .-. z1 ?*? y2)
      (z1 ?*? x2 .-. x1 ?*? z2)
      (x1 ?*? y2 .-. y1 ?*? x2)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds3D units1 space1)
    (Vector3D units2 space2)
    (VectorBounds3D units3 space1)
  where
  VectorBounds3D x1 y1 z1 `cross` Vector3D x2 y2 z2 =
    VectorBounds3D
      (y1 .*. z2 .-. z1 .*. y2)
      (z1 .*. x2 .-. x1 .*. z2)
      (x1 .*. y2 .-. y1 .*. x2)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorBounds3D units1 space1)
    (Vector3D units2 space2)
    (VectorBounds3D (units1 ?*? units2) space1)
  where
  VectorBounds3D x1 y1 z1 `cross_` Vector3D x2 y2 z2 =
    VectorBounds3D
      (y1 ?*? z2 .-. z1 ?*? y2)
      (z1 ?*? x2 .-. x1 ?*? z2)
      (x1 ?*? y2 .-. y1 ?*? x2)

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (VectorBounds3D units space2)
    (VectorBounds3D units space1)
  where
  Unit3D vector `cross` vectorBounds = vector `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorBounds3D units space1)
    (Direction3D space2)
    (VectorBounds3D units space1)
  where
  vectorBounds `cross` Unit3D vector = vectorBounds `cross` vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorBounds3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D units3 space1)
  where
  VectorBounds3D x1 y1 z1 `cross` VectorBounds3D x2 y2 z2 =
    VectorBounds3D
      (y1 .*. z2 .-. z1 .*. y2)
      (z1 .*. x2 .-. x1 .*. z2)
      (x1 .*. y2 .-. y1 .*. x2)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorBounds3D units1 space1)
    (VectorBounds3D units2 space2)
    (VectorBounds3D (units1 ?*? units2) space1)
  where
  VectorBounds3D x1 y1 z1 `cross_` VectorBounds3D x2 y2 z2 =
    VectorBounds3D
      (y1 ?*? z2 .-. z1 ?*? y2)
      (z1 ?*? x2 .-. x1 ?*? z2)
      (x1 ?*? y2 .-. y1 ?*? x2)

----- Bounds3D -----

type role Bounds3D phantom

-- | A bounding box in 3D.
newtype Bounds3D space
  = PositionBounds3D (VectorBounds3D Meters space)

{-# COMPLETE Bounds3D #-}

{-# INLINE Bounds3D #-}

-- | Construct a point from its X and Y coordinates.
pattern Bounds3D :: Bounds Meters -> Bounds Meters -> Bounds Meters -> Bounds3D space
pattern Bounds3D bx by bz <- PositionBounds3D (VectorBounds3D bx by bz)
  where
    Bounds3D bx by bz = PositionBounds3D (VectorBounds3D bx by bz)

deriving instance Show (Bounds3D space)

instance FFI (Bounds3D FFI.Space) where
  representation = FFI.classRepresentation "Bounds3D"

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Addition
    (Bounds3D space1)
    (Vector3D meters space2)
    (Bounds3D space1)
  where
  PositionBounds3D pb .+. v = PositionBounds3D (pb .+. v)

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Addition
    (Bounds3D space1)
    (VectorBounds3D meters space2)
    (Bounds3D space1)
  where
  PositionBounds3D pb .+. vb = PositionBounds3D (pb .+. vb)

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Subtraction
    (Bounds3D space1)
    (Vector3D meters space2)
    (Bounds3D space1)
  where
  PositionBounds3D pb .-. v = PositionBounds3D (pb .-. v)

instance
  ( space1 ~ space2
  , meters ~ Meters
  ) =>
  Subtraction
    (Bounds3D space1)
    (VectorBounds3D meters space2)
    (Bounds3D space1)
  where
  PositionBounds3D pb .-. vb = PositionBounds3D (pb .-. vb)

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (Bounds3D space2)
    (VectorBounds3D Meters space1)
  where
  Position3D p .-. PositionBounds3D pb = p .-. pb

instance
  space1 ~ space2 =>
  Subtraction
    (Bounds3D space1)
    (Point3D space2)
    (VectorBounds3D Meters space1)
  where
  PositionBounds3D pb .-. Position3D p = pb .-. p

instance
  space1 ~ space2 =>
  Subtraction
    (Bounds3D space1)
    (Bounds3D space2)
    (VectorBounds3D Meters space1)
  where
  PositionBounds3D pb1 .-. PositionBounds3D pb2 = pb1 .-. pb2

instance
  space1 ~ space2 =>
  Intersects (Point3D space1) (Bounds3D space2) Meters
  where
  Position3D p `intersects` PositionBounds3D pb = p `intersects` pb

instance
  space1 ~ space2 =>
  Intersects (Bounds3D space1) (Point3D space2) Meters
  where
  box `intersects` point = point `intersects` box

instance
  space1 ~ space2 =>
  Intersects (Bounds3D space1) (Bounds3D space2) Meters
  where
  PositionBounds3D pb1 `intersects` PositionBounds3D pb2 = pb1 `intersects` pb2

----- Axis3D -----

type role Axis3D phantom

-- | An axis in 3D, defined by an origin point and direction.
data Axis3D space
  = -- | Construct an axis from its origin point and direction.
    Axis3D
    { originPoint :: Point3D space
    , direction :: Direction3D space
    }

deriving instance Eq (Axis3D space)

deriving instance Show (Axis3D space)

instance FFI (Axis3D FFI.Space) where
  representation = FFI.classRepresentation "Axis3D"

----- Plane3D -----

type role Plane3D phantom phantom

type Plane3D :: Type -> Type -> Type

{-| A plane in 3D, defined by an origin point and two perpendicular X and Y directions.

The normal direction  of the plane is then defined as
the cross product of its X and Y directions.
-}
data Plane3D global local = Plane3D
  { originPoint :: Point3D global
  , orientation :: PlaneOrientation3D global
  }

deriving instance Eq (Plane3D global local)

deriving instance Ord (Plane3D global local)

deriving instance Show (Plane3D global local)

instance (global ~ FFI.Space, local ~ FFI.Space) => FFI (Plane3D global local) where
  representation = FFI.classRepresentation "Plane3D"

instance HasField "xDirection" (Plane3D global local) (Direction3D global) where
  getField = (.orientation.xDirection)

instance HasField "yDirection" (Plane3D global local) (Direction3D global) where
  getField = (.orientation.yDirection)

instance HasField "normalDirection" (Plane3D global local) (Direction3D global) where
  getField = (.orientation.normalDirection)

instance HasField "xAxis" (Plane3D global local) (Axis3D global) where
  getField plane = Axis3D plane.originPoint plane.xDirection

instance HasField "yAxis" (Plane3D global local) (Axis3D global) where
  getField plane = Axis3D plane.originPoint plane.yDirection

instance HasField "normalAxis" (Plane3D global local) (Axis3D global) where
  getField plane = Axis3D plane.originPoint plane.normalDirection

----- Frame3D -----

type role Frame3D phantom phantom

-- | A frame of reference in 3D, defined by an origin point and orientation.
type Frame3D :: Type -> Type -> Type
data Frame3D global local = Frame3D
  { originPoint :: Point3D global
  , orientation :: Orientation3D global
  }

instance HasField "rightwardDirection" (Frame3D global local) (Direction3D global) where
  getField = (.orientation.rightwardDirection)

instance HasField "leftwardDirection" (Frame3D global local) (Direction3D global) where
  getField = (.orientation.leftwardDirection)

instance HasField "forwardDirection" (Frame3D global local) (Direction3D global) where
  getField = (.orientation.forwardDirection)

instance HasField "backwardDirection" (Frame3D global local) (Direction3D global) where
  getField = (.orientation.backwardDirection)

instance HasField "upwardDirection" (Frame3D global local) (Direction3D global) where
  getField = (.orientation.upwardDirection)

instance HasField "downwardDirection" (Frame3D global local) (Direction3D global) where
  getField = (.orientation.downwardDirection)

instance HasField "rightwardAxis" (Frame3D global local) (Axis3D global) where
  getField frame = Axis3D frame.originPoint frame.rightwardDirection

instance HasField "leftwardAxis" (Frame3D global local) (Axis3D global) where
  getField frame = Axis3D frame.originPoint frame.leftwardDirection

instance HasField "forwardAxis" (Frame3D global local) (Axis3D global) where
  getField frame = Axis3D frame.originPoint frame.forwardDirection

instance HasField "backwardAxis" (Frame3D global local) (Axis3D global) where
  getField frame = Axis3D frame.originPoint frame.backwardDirection

instance HasField "upwardAxis" (Frame3D global local) (Axis3D global) where
  getField frame = Axis3D frame.originPoint frame.upwardDirection

instance HasField "downwardAxis" (Frame3D global local) (Axis3D global) where
  getField frame = Axis3D frame.originPoint frame.downwardDirection

instance HasField "backwardOrientation" (Frame3D global local) (Orientation3D global) where
  getField = (.orientation.backwardOrientation)

instance HasField "rightwardOrientation" (Frame3D global local) (Orientation3D global) where
  getField = (.orientation.rightwardOrientation)

instance HasField "leftwardOrientation" (Frame3D global local) (Orientation3D global) where
  getField = (.orientation.leftwardOrientation)

instance HasField "upwardOrientation" (Frame3D global local) (Orientation3D global) where
  getField = (.orientation.upwardOrientation)

instance HasField "downwardOrientation" (Frame3D global local) (Orientation3D global) where
  getField = (.orientation.downwardOrientation)

deriving instance Eq (Frame3D global local)

deriving instance Show (Frame3D global local)

instance FFI (Frame3D FFI.Space local) where
  representation = FFI.classRepresentation "Frame3D"

----- Transform3D -----

type role Transform3D phantom phantom

type Transform3D :: Type -> Type -> Type
data Transform3D tag space
  = Transform3D
      (Point3D space)
      (Vector3D Unitless space)
      (Vector3D Unitless space)
      (Vector3D Unitless space)

deriving instance Eq (Transform3D tag space)

deriving instance Ord (Transform3D tag space)

deriving instance Show (Transform3D tag space)

instance
  space1 ~ space2 =>
  Multiplication
    (Transform3D tag space1)
    (Vector3D units space2)
    (Vector3D units space1)
  where
  transform .*. vector = vector .*. transform

instance
  space1 ~ space2 =>
  Multiplication
    (Vector3D units space1)
    (Transform3D tag space2)
    (Vector3D units space1)
  where
  Vector3D vx vy vz .*. Transform3D _ i j k = vx .*. i .+. vy .*. j .+. vz .*. k

instance
  space1 ~ space2 =>
  Multiplication
    (Point3D space1)
    (Transform3D tag space2)
    (Point3D space1)
  where
  Point3D px py pz .*. Transform3D p0 i j k = p0 .+. px .*. i .+. py .*. j .+. pz .*. k

instance
  space1 ~ space2 =>
  Multiplication
    (Transform3D tag space1)
    (Point3D space2)
    (Point3D space1)
  where
  transform .*. point = point .*. transform

instance
  ( Composition tag1 tag2 tag3
  , space1 ~ space2
  ) =>
  Composition
    (Transform3D tag1 space1)
    (Transform3D tag2 space2)
    (Transform3D tag3 space1)
  where
  transform2 `compose` transform1 =
    Transform3D
      (Point3D Quantity.zero Quantity.zero Quantity.zero .*. transform1 .*. transform2)
      (Vector3D 1 0 0 .*. transform1 .*. transform2)
      (Vector3D 0 1 0 .*. transform1 .*. transform2)
      (Vector3D 0 0 1 .*. transform1 .*. transform2)
