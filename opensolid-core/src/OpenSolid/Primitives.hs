module OpenSolid.Primitives
  ( Vector2d (Vector2d)
  , Direction2d (Unit2d, Direction2d)
  , Basis2d (Basis2d)
  , Point2d (Point2d)
  , VectorBounds2d (VectorBounds2d)
  , Bounds2d (Bounds2d)
  , Axis2d (Axis2d)
  , Frame2d (Frame2d)
  , Transform2d (Transform2d)
  , Vector3d (Vector3d)
  , Direction3d (Unit3d, Direction3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Basis3d (Basis3d, rightwardDirection, forwardDirection, upwardDirection)
  , Point3d (Point3d)
  , VectorBounds3d (VectorBounds3d)
  , Bounds3d (Bounds3d)
  , Axis3d (Axis3d)
  , Plane3d (Plane3d)
  , Frame3d (Frame3d)
  , Transform3d (Transform3d)
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds (Bounds)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.HasZero (HasZero)
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Units (Meters, Radians, SquareMeters)
import OpenSolid.Units qualified as Units

----- Vector2d -----

data Vector2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a vector from its X and Y components.
  Vector2d :: Qty units -> Qty units -> Vector2d (space @ units)

deriving instance Eq (Vector2d (space @ units))

deriving instance Ord (Vector2d (space @ units))

deriving instance Show (Qty units) => Show (Vector2d (space @ units))

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
  coerce (Vector2d vx vy) = Vector2d (Qty.coerce vx) (Qty.coerce vy)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) units1
  where
  Vector2d x1 y1 ~= Vector2d x2 y2 = Qty.hypot2 (x2 - x1) (y2 - y1) ~= Qty.zero

instance HasZero (Vector2d (space @ units)) where
  zero = Vector2d Qty.zero Qty.zero

instance Negation (Vector2d (space @ units)) where
  negate (Vector2d vx vy) = Vector2d (negate vx) (negate vy)

instance Multiplication Sign (Vector2d (space @ units)) (Vector2d (space @ units)) where
  Positive * vector = vector
  Negative * vector = -vector

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
  Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  Multiplication'
    (Qty units1)
    (Vector2d (space @ units2))
    (Vector2d (space @ (units1 :*: units2)))
  where
  scale .*. Vector2d vx vy = Vector2d (scale .*. vx) (scale .*. vy)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Vector2d (space @ units2)) (Vector2d (space @ units3))
  where
  scale * Vector2d vx vy = Vector2d (scale * vx) (scale * vy)

instance
  Multiplication'
    (Vector2d (space @ units1))
    (Qty units2)
    (Vector2d (space @ (units1 :*: units2)))
  where
  Vector2d vx vy .*. scale = Vector2d (vx .*. scale) (vy .*. scale)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))
  where
  Vector2d vx vy * scale = Vector2d (vx * scale) (vy * scale)

instance
  Multiplication'
    (Bounds units1)
    (Vector2d (space @ units2))
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  bounds .*. Vector2d vx vy = VectorBounds2d (bounds .*. vx) (bounds .*. vy)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Vector2d (space @ units2)) (VectorBounds2d (space @ units3))
  where
  bounds * Vector2d vx vy = VectorBounds2d (bounds * vx) (bounds * vy)

instance
  Multiplication'
    (Vector2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  Vector2d vx vy .*. bounds = VectorBounds2d (vx .*. bounds) (vy .*. bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Bounds units2) (VectorBounds2d (space @ units3))
  where
  Vector2d vx vy * bounds = VectorBounds2d (vx * bounds) (vy * bounds)

instance
  Division'
    (Vector2d (space @ units1))
    (Qty units2)
    (Vector2d (space @ (units1 :/: units2)))
  where
  Vector2d vx vy ./. scale = Vector2d (vx ./. scale) (vy ./. scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))
  where
  Vector2d vx vy / scale = Vector2d (vx / scale) (vy / scale)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Qty (units1 :*: units2))
  where
  Vector2d x1 y1 `dot'` Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Qty units3)
  where
  Vector2d x1 y1 `dot` Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Qty units)
  where
  v `dot` Unit2d d = v `dot` d

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Qty units)
  where
  Unit2d d `dot` v = d `dot` v

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Qty (units1 :*: units2))
  where
  Vector2d x1 y1 `cross'` Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Qty units3)
  where
  Vector2d x1 y1 `cross` Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Qty units)
  where
  v1 `cross` Unit2d v2 = v1 `cross` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Qty units)
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
pattern Direction2d :: Float -> Float -> Direction2d space
pattern Direction2d dX dY = Unit2d (Vector2d dX dY)

instance HasUnits (Direction2d space) Unitless (Direction2d space)

instance FFI (Direction2d space) where
  representation = FFI.classRepresentation "Direction2d"

instance space1 ~ space2 => Units.Coercion (Direction2d space1) (Direction2d space2) where
  coerce = identity

instance
  space1 ~ space2 =>
  ApproximateEquality (Direction2d space1) (Direction2d space2) Radians
  where
  d1 ~= d2 = Angle.atan2 (d1 `cross` d2) (d1 `dot` d2) ~= Angle.zero

instance Negation (Direction2d space) where
  negate (Unit2d v) = Unit2d -v

instance Multiplication' Sign (Direction2d space) (Direction2d space) where
  Positive .*. direction = direction
  Negative .*. direction = -direction

instance Multiplication Sign (Direction2d space) (Direction2d space) where
  Positive * direction = direction
  Negative * direction = -direction

instance Multiplication' (Direction2d space) Sign (Direction2d space) where
  direction .*. Positive = direction
  direction .*. Negative = -direction

instance Multiplication (Direction2d space) Sign (Direction2d space) where
  direction * Positive = direction
  direction * Negative = -direction

instance Multiplication (Qty units) (Direction2d space) (Vector2d (space @ units)) where
  scale * Unit2d v = scale * v

instance Multiplication (Direction2d space) (Qty units) (Vector2d (space @ units)) where
  Unit2d v * scale = v * scale

instance space1 ~ space2 => DotMultiplication (Direction2d space1) (Direction2d space2) Float where
  Unit2d v1 `dot` Unit2d v2 = v1 `dot` v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Direction2d space2) Float
  where
  Unit2d v1 `cross` Unit2d v2 = v1 `cross` v2

----- Basis2d -----

type role Basis2d nominal nominal

type Basis2d :: Type -> LocalSpace -> Type
data Basis2d space defines where
  Basis2d :: Direction2d space -> Direction2d space -> Basis2d space defines

deriving instance Eq (Basis2d space defines)

deriving instance Show (Basis2d space defines)

----- Point2d -----

data Point2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a point from its X and Y coordinates.
  Point2d :: Qty units -> Qty units -> Point2d (space @ units)

deriving instance Eq (Point2d (space @ units))

deriving instance Ord (Point2d (space @ units))

deriving instance Show (Qty units) => Show (Point2d (space @ units))

instance FFI (Point2d (space @ Meters)) where
  representation = FFI.classRepresentation "Point2d"

instance FFI (Point2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvPoint"

instance HasUnits (Point2d (space @ units)) units (Point2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Point2d (space1 @ unitsA)) (Point2d (space2 @ unitsB))
  where
  coerce (Point2d px py) = Point2d (Qty.coerce px) (Qty.coerce py)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Point2d px py + Vector2d vx vy = Point2d (px + vx) (py + vy)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Point2d px py - Vector2d vx vy = Point2d (px - vx) (py - vy)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Point2d x1 y1 - Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Point2d px py + VectorBounds2d vx vy = Bounds2d (px + vx) (py + vy)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Point2d px py - VectorBounds2d vx vy = Bounds2d (px - vx) (py - vy)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Point2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  Point2d x1 y1 ~= Point2d x2 y2 = Qty.hypot2 (x2 - x1) (y2 - y1) ~= Qty.zero

----- VectorBounds2d -----

data VectorBounds2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a vector bounding box from its X and Y coordinate bounds.
  VectorBounds2d :: Bounds units -> Bounds units -> VectorBounds2d (space @ units)

deriving instance Show (Qty units) => Show (VectorBounds2d (space @ units))

instance HasUnits (VectorBounds2d (space @ units)) units (VectorBounds2d (space @ Unitless))

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
  negate (VectorBounds2d x y) = VectorBounds2d (negate x) (negate y)

instance Multiplication Sign (VectorBounds2d (space @ units)) (VectorBounds2d (space @ units)) where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = -vectorBounds

instance Multiplication (VectorBounds2d (space @ units)) Sign (VectorBounds2d (space @ units)) where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = -vectorBounds

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
  Multiplication'
    (Qty units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  value .*. VectorBounds2d x y = VectorBounds2d (value .*. x) (value .*. y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorBounds2d (space @ units2)) (VectorBounds2d (space @ units3))
  where
  value * VectorBounds2d x y = VectorBounds2d (value * x) (value * y)

instance
  Multiplication'
    (VectorBounds2d (space @ units1))
    (Qty units2)
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  VectorBounds2d x y .*. value = VectorBounds2d (x .*. value) (y .*. value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds2d (space @ units1)) (Qty units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * value = VectorBounds2d (x * value) (y * value)

instance
  Multiplication'
    (Bounds units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  bounds .*. VectorBounds2d x y = VectorBounds2d (bounds .*. x) (bounds .*. y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (VectorBounds2d (space @ units2)) (VectorBounds2d (space @ units3))
  where
  bounds * VectorBounds2d x y = VectorBounds2d (bounds * x) (bounds * y)

instance
  Multiplication'
    (VectorBounds2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  VectorBounds2d x y .*. bounds = VectorBounds2d (x .*. bounds) (y .*. bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds2d (space @ units1)) (Bounds units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * bounds = VectorBounds2d (x * bounds) (y * bounds)

instance
  Division'
    (VectorBounds2d (space @ units1))
    (Qty units2)
    (VectorBounds2d (space @ (units1 :/: units2)))
  where
  VectorBounds2d x y ./. value = VectorBounds2d (x ./. value) (y ./. value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2d (space @ units1)) (Qty units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y / value = VectorBounds2d (x / value) (y / value)

instance
  Division'
    (VectorBounds2d (space @ units1))
    (Bounds units2)
    (VectorBounds2d (space @ (units1 :/: units2)))
  where
  VectorBounds2d x y ./. bounds = VectorBounds2d (x ./. bounds) (y ./. bounds)

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
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 :*: units2))
  where
  Vector2d x1 y1 `dot'` VectorBounds2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorBounds2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Bounds units3)
  where
  VectorBounds2d x1 y1 `dot` Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds (units1 :*: units2))
  where
  VectorBounds2d x1 y1 `dot'` Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

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
  DotMultiplication'
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 :*: units2))
  where
  VectorBounds2d x1 y1 `dot'` VectorBounds2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication (Vector2d (space1 @ units1)) (VectorBounds2d (space2 @ units2)) (Bounds units3)
  where
  Vector2d x1 y1 `cross` VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 :*: units2))
  where
  Vector2d x1 y1 `cross'` VectorBounds2d x2 y2 = x1 .*. y2 - y1 .*. x2

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
  CrossMultiplication'
    (VectorBounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds (units1 :*: units2))
  where
  VectorBounds2d x1 y1 `cross'` Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

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
  CrossMultiplication'
    (VectorBounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds (units1 :*: units2))
  where
  VectorBounds2d x1 y1 `cross'` VectorBounds2d x2 y2 = x1 .*. y2 - y1 .*. x2

----- Bounds2d -----

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a bounding box from its X and Y coordinate bounds.
  Bounds2d ::
    Bounds units ->
    Bounds units ->
    Bounds2d (space @ units)

deriving instance Show (Qty units) => Show (Bounds2d (space @ units))

instance HasUnits (Bounds2d (space @ units)) units (Bounds2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds2d (space1 @ unitsA)) (Bounds2d (space2 @ unitsB))
  where
  coerce (Bounds2d x y) = Bounds2d (Units.coerce x) (Units.coerce y)

instance FFI (Bounds2d (space @ Meters)) where
  representation = FFI.classRepresentation "Bounds2d"

instance FFI (Bounds2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvBounds"

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Point2d px py - Bounds2d bx by = VectorBounds2d (px - bx) (py - by)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Bounds2d bx by - Point2d px py = VectorBounds2d (bx - px) (by - py)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - Bounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 + Vector2d x2 y2 = Bounds2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 + VectorBounds2d x2 y2 = Bounds2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - Vector2d x2 y2 = Bounds2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - VectorBounds2d x2 y2 = Bounds2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Point2d px py ~= Bounds2d bx by = px ~= bx && py ~= by

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  box ~= point = point ~= box

instance
  (units1 ~ units2, space1 ~ space2) =>
  Intersects (Point2d (space1 @ units1)) (Axis2d (space2 @ units2)) units1
  where
  p ^ (Axis2d p0 d) = (p - p0) `cross` d ~= Qty.zero

instance
  (units1 ~ units2, space1 ~ space2) =>
  Intersects (Axis2d (space2 @ units2)) (Point2d (space1 @ units1)) units1
  where
  axis ^ point = point ^ axis

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Point2d px py ^ Bounds2d bx by = px ^ bx && py ^ by

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  box ^ point = point ^ box

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Bounds2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Bounds2d x1 y1 ^ Bounds2d x2 y2 = x1 ^ x2 && y1 ^ y2

----- Axis2d -----

-- | An axis in 2D, defined by an origin point and direction.
data Axis2d (coordinateSystem :: CoordinateSystem) where
  -- | Construct an axis from its origin point and direction.
  Axis2d ::
    Point2d (space @ units) ->
    Direction2d space ->
    Axis2d (space @ units)

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Qty units) => Show (Axis2d (space @ units))

instance HasUnits (Axis2d (space @ units)) units (Axis2d (space @ Unitless))

instance FFI (Axis2d (space @ Meters)) where
  representation = FFI.classRepresentation "Axis2d"

instance FFI (Axis2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvAxis"

----- Frame2d -----

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    Point2d (space @ units) ->
    Basis2d space defines ->
    Frame2d (space @ units) defines

deriving instance Eq (Frame2d (space @ units) defines)

deriving instance Show (Qty units) => Show (Frame2d (space @ units) defines)

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

deriving instance Show (Qty units) => Show (Transform2d tag (space @ units))

instance HasUnits (Transform2d tag (space @ units)) units (Transform2d tag (space @ Unitless))

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
  transform1 >> transform2 =
    Transform2d
      (Point2d Qty.zero Qty.zero * transform1 * transform2)
      (Vector2d 1.0 0.0 * transform1 * transform2)
      (Vector2d 0.0 1.0 * transform1 * transform2)

----- Vector3d -----

data Vector3d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a vector from its XYZ components.
  Vector3d :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)

deriving instance Eq (Vector3d (space @ units))

deriving instance Ord (Vector3d (space @ units))

deriving instance Show (Qty units) => Show (Vector3d (space @ units))

instance FFI (Vector3d (space @ Unitless)) where
  representation = FFI.classRepresentation "Vector3d"

instance FFI (Vector3d (space @ Meters)) where
  representation = FFI.classRepresentation "Displacement3d"

instance FFI (Vector3d (space @ SquareMeters)) where
  representation = FFI.classRepresentation "AreaVector3d"

instance HasUnits (Vector3d (space @ units)) units (Vector3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion
    (Vector3d (space1 @ unitsA))
    (Vector3d (space2 @ unitsB))
  where
  coerce (Vector3d vx vy vz) = Vector3d (Qty.coerce vx) (Qty.coerce vy) (Qty.coerce vz)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Vector3d (space1 @ units1)) (Vector3d (space2 @ units2)) units1
  where
  Vector3d x1 y1 z1 ~= Vector3d x2 y2 z2 = Qty.hypot3 (x2 - x1) (y2 - y1) (z2 - z1) ~= Qty.zero

instance HasZero (Vector3d (space @ units)) where
  zero = Vector3d Qty.zero Qty.zero Qty.zero

instance Negation (Vector3d (space @ units)) where
  negate (Vector3d vx vy vz) = Vector3d (negate vx) (negate vy) (negate vz)

instance Multiplication Sign (Vector3d (space @ units)) (Vector3d (space @ units)) where
  Positive * vector = vector
  Negative * vector = -vector

instance Multiplication (Vector3d (space @ units)) Sign (Vector3d (space @ units)) where
  vector * Positive = vector
  vector * Negative = -vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Vector3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Vector3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  Multiplication'
    (Qty units1)
    (Vector3d (space @ units2))
    (Vector3d (space @ (units1 :*: units2)))
  where
  scale .*. Vector3d vx vy vz = Vector3d (scale .*. vx) (scale .*. vy) (scale .*. vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Vector3d (space @ units2)) (Vector3d (space @ units3))
  where
  scale * Vector3d vx vy vz = Vector3d (scale * vx) (scale * vy) (scale * vz)

instance
  Multiplication'
    (Vector3d (space @ units1))
    (Qty units2)
    (Vector3d (space @ (units1 :*: units2)))
  where
  Vector3d vx vy vz .*. scale = Vector3d (vx .*. scale) (vy .*. scale) (vz .*. scale)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))
  where
  Vector3d vx vy vz * scale = Vector3d (vx * scale) (vy * scale) (vz * scale)

instance
  Multiplication'
    (Bounds units1)
    (Vector3d (space @ units2))
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  bounds .*. Vector3d vx vy vz = VectorBounds3d (bounds .*. vx) (bounds .*. vy) (bounds .*. vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Vector3d (space @ units2)) (VectorBounds3d (space @ units3))
  where
  bounds * Vector3d vx vy vz = VectorBounds3d (bounds * vx) (bounds * vy) (bounds * vz)

instance
  Multiplication'
    (Vector3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  Vector3d vx vy vz .*. bounds = VectorBounds3d (vx .*. bounds) (vy .*. bounds) (vz .*. bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Bounds units2) (VectorBounds3d (space @ units3))
  where
  Vector3d vx vy vz * bounds = VectorBounds3d (vx * bounds) (vy * bounds) (vz * bounds)

instance
  Division'
    (Vector3d (space @ units1))
    (Qty units2)
    (Vector3d (space @ (units1 :/: units2)))
  where
  Vector3d vx vy vz ./. scale = Vector3d (vx ./. scale) (vy ./. scale) (vz ./. scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))
  where
  Vector3d vx vy vz / scale = Vector3d (vx / scale) (vy / scale) (vz / scale)

instance
  space ~ space_ =>
  DotMultiplication'
    (Vector3d (space @ units1))
    (Vector3d (space_ @ units2))
    (Qty (units1 :*: units2))
  where
  Vector3d x1 y1 z1 `dot'` Vector3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (Vector3d (space @ units1)) (Vector3d (space_ @ units2)) (Qty units3)
  where
  Vector3d x1 y1 z1 `dot` Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space ~ space_ =>
  DotMultiplication (Vector3d (space @ units)) (Direction3d space_) (Qty units)
  where
  v1 `dot` Unit3d v2 = v1 `dot` v2

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (Vector3d (space_ @ units)) (Qty units)
  where
  Unit3d v1 `dot` v2 = v1 `dot` v2

instance
  space ~ space_ =>
  CrossMultiplication'
    (Vector3d (space @ units1))
    (Vector3d (space_ @ units2))
    (Vector3d (space @ (units1 :*: units2)))
  where
  Vector3d x1 y1 z1 `cross'` Vector3d x2 y2 z2 =
    Vector3d
      (y1 .*. z2 - z1 .*. y2)
      (z1 .*. x2 - x1 .*. z2)
      (x1 .*. y2 - y1 .*. x2)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (Vector3d (space_ @ units2))
    (Vector3d (space @ units3))
  where
  Vector3d x1 y1 z1 `cross` Vector3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space ~ space_ =>
  CrossMultiplication (Vector3d (space @ units)) (Direction3d space_) (Vector3d (space @ units))
  where
  v1 `cross` Unit3d v2 = v1 `cross` v2

instance
  space ~ space_ =>
  CrossMultiplication (Direction3d space) (Vector3d (space_ @ units)) (Vector3d (space @ units))
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
pattern Direction3d :: Float -> Float -> Float -> Direction3d space
pattern Direction3d dR dF dU = Unit3d (Vector3d dR dF dU)

instance FFI (Direction3d space) where
  representation = FFI.classRepresentation "Direction3d"

instance HasUnits (Direction3d space) Unitless (Direction3d space)

instance space1 ~ space2 => Units.Coercion (Direction3d space1) (Direction3d space2) where
  coerce = identity

instance
  space1 ~ space2 =>
  ApproximateEquality (Direction3d space1) (Direction3d space2) Radians
  where
  d1 ~= d2 = do
    let parallel = d1 `dot` d2
    let Vector3d cx cy cz = d1 `cross` d2
    let perpendicular = Qty.hypot3 cx cy cz
    Angle.atan2 perpendicular parallel ~= Qty.zero

instance Negation (Direction3d space) where
  negate (Unit3d vector) = Unit3d (negate vector)

instance Multiplication Sign (Direction3d space) (Direction3d space) where
  Positive * direction = direction
  Negative * direction = -direction

instance Multiplication' Sign (Direction3d space) (Direction3d space) where
  Positive .*. direction = direction
  Negative .*. direction = -direction

instance Multiplication (Direction3d space) Sign (Direction3d space) where
  direction * Positive = direction
  direction * Negative = -direction

instance Multiplication' (Direction3d space) Sign (Direction3d space) where
  direction .*. Positive = direction
  direction .*. Negative = -direction

instance Multiplication (Qty units) (Direction3d space) (Vector3d (space @ units)) where
  scale * Unit3d vector = scale * vector

instance Multiplication (Direction3d space) (Qty units) (Vector3d (space @ units)) where
  Unit3d vector * scale = vector * scale

instance Multiplication (Bounds units) (Direction3d space) (VectorBounds3d (space @ units)) where
  bounds * Unit3d vector = bounds * vector

instance Multiplication (Direction3d space) (Bounds units) (VectorBounds3d (space @ units)) where
  Unit3d vector * bounds = vector * bounds

instance space1 ~ space2 => DotMultiplication (Direction3d space1) (Direction3d space2) Float where
  Unit3d vector1 `dot` Unit3d vector2 = vector1 `dot` vector2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction3d space1) (Direction3d space2) (Vector3d (space1 @ Unitless))
  where
  Unit3d vector1 `cross` Unit3d vector2 = vector1 `cross` vector2

----- PlanarBasis3d -----

-- | A pair of perpendicular X and Y directions defining the orientation of a plane in 3D.
type PlanarBasis3d :: Type -> LocalSpace -> Type
data PlanarBasis3d space defines where
  PlanarBasis3d :: Direction3d space -> Direction3d space -> PlanarBasis3d space defines

deriving instance Eq (PlanarBasis3d space defines)

deriving instance Ord (PlanarBasis3d space defines)

deriving instance Show (PlanarBasis3d space defines)

instance FFI (PlanarBasis3d space defines) where
  representation = FFI.classRepresentation "PlanarBasis3d"

----- Basis3d -----

-- | A set of cardinal directions (forward, upward etc.) defining a 3D orientation.
type Basis3d :: Type -> LocalSpace -> Type
data Basis3d space defines where
  Basis3d ::
    { rightwardDirection :: Direction3d space
    -- ^ Get the rightward direction of a basis.
    , forwardDirection :: Direction3d space
    -- ^ Get the forward direction of a basis.
    , upwardDirection :: Direction3d space
    -- ^ Get the upward direction of a basis.
    } ->
    Basis3d space defines

deriving instance Eq (Basis3d space defines)

deriving instance Show (Basis3d space defines)

instance FFI (Basis3d space defines) where
  representation = FFI.classRepresentation "Basis3d"

----- Point3d -----

data Point3d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a point from its XYZ coordinates.
  Point3d :: Qty units -> Qty units -> Qty units -> Point3d (space @ units)

deriving instance Eq (Point3d (space @ units))

deriving instance Ord (Point3d (space @ units))

deriving instance Show (Qty units) => Show (Point3d (space @ units))

instance FFI (Point3d (space @ Meters)) where
  representation = FFI.classRepresentation "Point3d"

instance HasUnits (Point3d (space @ units)) units (Point3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Point3d (space1 @ unitsA)) (Point3d (space2 @ unitsB))
  where
  coerce (Point3d px py pz) = Point3d (Qty.coerce px) (Qty.coerce py) (Qty.coerce pz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Point3d px py pz + Vector3d vx vy vz = Point3d (px + vx) (py + vy) (pz + vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))
  where
  Point3d px py pz - Vector3d vx vy vz = Point3d (px - vx) (py - vy) (pz - vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (Vector3d (space1 @ units1))
  where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Point3d px py pz + VectorBounds3d vx vy vz = Bounds3d (px + vx) (py + vy) (pz + vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Point3d px py pz - VectorBounds3d vx vy vz = Bounds3d (px - vx) (py - vy) (pz - vz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Point3d (space1 @ units1)) (Point3d (space2 @ units2)) units1
  where
  Point3d x1 y1 z1 ~= Point3d x2 y2 z2 = Qty.hypot3 (x2 - x1) (y2 - y1) (z2 - z1) ~= Qty.zero

----- VectorBounds3d -----

data VectorBounds3d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a vector bounding box from its XYZ coordinate bounds.
  VectorBounds3d ::
    Bounds units ->
    Bounds units ->
    Bounds units ->
    VectorBounds3d (space @ units)

deriving instance Show (Qty units) => Show (VectorBounds3d (space @ units))

instance HasUnits (VectorBounds3d (space @ units)) units (VectorBounds3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorBounds3d (space1 @ unitsA)) (VectorBounds3d (space2 @ unitsB))
  where
  coerce (VectorBounds3d x y z) = VectorBounds3d (Units.coerce x) (Units.coerce y) (Units.coerce z)

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
  negate (VectorBounds3d x y z) = VectorBounds3d (negate x) (negate y) (negate z)

instance Multiplication Sign (VectorBounds3d (space @ units)) (VectorBounds3d (space @ units)) where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = -vectorBounds

instance Multiplication (VectorBounds3d (space @ units)) Sign (VectorBounds3d (space @ units)) where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = -vectorBounds

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

instance
  Multiplication'
    (Qty units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  value .*. VectorBounds3d x y z = VectorBounds3d (value .*. x) (value .*. y) (value .*. z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorBounds3d (space @ units2)) (VectorBounds3d (space @ units3))
  where
  value * VectorBounds3d x y z = VectorBounds3d (value * x) (value * y) (value * z)

instance
  Multiplication'
    (VectorBounds3d (space @ units1))
    (Qty units2)
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  VectorBounds3d x y z .*. value = VectorBounds3d (x .*. value) (y .*. value) (z .*. value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds3d (space @ units1)) (Qty units2) (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z * value = VectorBounds3d (x * value) (y * value) (z * value)

instance
  Multiplication'
    (Bounds units1)
    (VectorBounds3d (space @ units2))
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  bounds .*. VectorBounds3d x y z = VectorBounds3d (bounds .*. x) (bounds .*. y) (bounds .*. z)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (VectorBounds3d (space @ units2)) (VectorBounds3d (space @ units3))
  where
  bounds * VectorBounds3d x y z = VectorBounds3d (bounds * x) (bounds * y) (bounds * z)

instance
  Multiplication'
    (VectorBounds3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  VectorBounds3d x y z .*. bounds = VectorBounds3d (x .*. bounds) (y .*. bounds) (z .*. bounds)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds3d (space @ units1)) (Bounds units2) (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z * bounds = VectorBounds3d (x * bounds) (y * bounds) (z * bounds)

instance
  Division'
    (VectorBounds3d (space @ units1))
    (Qty units2)
    (VectorBounds3d (space @ (units1 :/: units2)))
  where
  VectorBounds3d x y z ./. value = VectorBounds3d (x ./. value) (y ./. value) (z ./. value)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3d (space @ units1)) (Qty units2) (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z / value = VectorBounds3d (x / value) (y / value) (z / value)

instance
  Division'
    (VectorBounds3d (space @ units1))
    (Bounds units2)
    (VectorBounds3d (space @ (units1 :/: units2)))
  where
  VectorBounds3d x y z ./. bounds = VectorBounds3d (x ./. bounds) (y ./. bounds) (z ./. bounds)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds3d (space @ units1)) (Bounds units2) (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x y z / bounds = VectorBounds3d (x / bounds) (y / bounds) (z / bounds)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (Vector3d (space @ units1)) (VectorBounds3d (space_ @ units2)) (Bounds units3)
  where
  Vector3d x1 y1 z1 `dot` VectorBounds3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space ~ space_ =>
  DotMultiplication'
    (Vector3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (Bounds (units1 :*: units2))
  where
  Vector3d x1 y1 z1 `dot'` VectorBounds3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (VectorBounds3d (space @ units1)) (Vector3d (space_ @ units2)) (Bounds units3)
  where
  VectorBounds3d x1 y1 z1 `dot` Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorBounds3d (space @ units1))
    (Vector3d (space_ @ units2))
    (Bounds (units1 :*: units2))
  where
  VectorBounds3d x1 y1 z1 `dot'` Vector3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (VectorBounds3d (space_ @ units)) (Bounds units)
  where
  Unit3d vector `dot` vectorBounds = vector `dot` vectorBounds

instance
  space ~ space_ =>
  DotMultiplication (VectorBounds3d (space @ units)) (Direction3d space_) (Bounds units)
  where
  vectorBounds `dot` Unit3d vector = vectorBounds `dot` vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (Bounds units3)
  where
  VectorBounds3d x1 y1 z1 `dot` VectorBounds3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (Bounds (units1 :*: units2))
  where
  VectorBounds3d x1 y1 z1 `dot'` VectorBounds3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (VectorBounds3d (space @ units3))
  where
  Vector3d x1 y1 z1 `cross` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space ~ space_ =>
  CrossMultiplication'
    (Vector3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  Vector3d x1 y1 z1 `cross'` VectorBounds3d x2 y2 z2 =
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
  where
  VectorBounds3d x1 y1 z1 `cross` Vector3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorBounds3d (space @ units1))
    (Vector3d (space_ @ units2))
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  VectorBounds3d x1 y1 z1 `cross'` Vector3d x2 y2 z2 =
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
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (VectorBounds3d (space @ units3))
  where
  VectorBounds3d x1 y1 z1 `cross` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorBounds3d (space @ units1))
    (VectorBounds3d (space_ @ units2))
    (VectorBounds3d (space @ (units1 :*: units2)))
  where
  VectorBounds3d x1 y1 z1 `cross'` VectorBounds3d x2 y2 z2 =
    VectorBounds3d
      (y1 .*. z2 - z1 .*. y2)
      (z1 .*. x2 - x1 .*. z2)
      (x1 .*. y2 - y1 .*. x2)

----- Bounds3d -----

data Bounds3d (coordinateSystem :: CoordinateSystem) where
  -- | Construct a bounding box from its XYZ coordinate bounds.
  Bounds3d ::
    Bounds units ->
    Bounds units ->
    Bounds units ->
    Bounds3d (space @ units)

deriving instance Show (Qty units) => Show (Bounds3d (space @ units))

instance FFI (Bounds3d (space @ Meters)) where
  representation = FFI.classRepresentation "Bounds3d"

instance HasUnits (Bounds3d (space @ units)) units (Bounds3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds3d (space1 @ unitsA)) (Bounds3d (space2 @ unitsB))
  where
  coerce (Bounds3d x y z) = Bounds3d (Units.coerce x) (Units.coerce y) (Units.coerce z)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Bounds3d x1 y1 z1 + Vector3d x2 y2 z2 = Bounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Bounds3d x1 y1 z1 + VectorBounds3d x2 y2 z2 = Bounds3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Bounds3d x1 y1 z1 - Vector3d x2 y2 z2 = Bounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (VectorBounds3d (space2 @ units2))
    (Bounds3d (space1 @ units1))
  where
  Bounds3d x1 y1 z1 - VectorBounds3d x2 y2 z2 = Bounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Bounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  Point3d px py pz - Bounds3d bx by bz = VectorBounds3d (px - bx) (py - by) (pz - bz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  Bounds3d bx by bz - Point3d px py pz = VectorBounds3d (bx - px) (by - py) (bz - pz)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds3d (space1 @ units1))
    (Bounds3d (space2 @ units2))
    (VectorBounds3d (space1 @ units1))
  where
  Bounds3d x1 y1 z1 - Bounds3d x2 y2 z2 = VectorBounds3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Point3d (space1 @ units1)) (Bounds3d (space2 @ units2)) units1
  where
  Point3d px py pz ^ Bounds3d bx by bz = px ^ bx && py ^ by && pz ^ bz

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
  Bounds3d x1 y1 z1 ^ Bounds3d x2 y2 z2 = x1 ^ x2 && y1 ^ y2 && z1 ^ z2

----- Axis3d -----

-- | An axis in 3D, defined by an origin point and direction.
data Axis3d (coordinateSystem :: CoordinateSystem) where
  Axis3d ::
    Point3d (space @ units) ->
    Direction3d space ->
    Axis3d (space @ units)

deriving instance Eq (Axis3d (space @ units))

deriving instance Show (Qty units) => Show (Axis3d (space @ units))

instance FFI (Axis3d (space @ Meters)) where
  representation = FFI.classRepresentation "Axis3d"

----- Plane3d -----

type role Plane3d nominal nominal

type Plane3d :: CoordinateSystem -> LocalSpace -> Type

{-| A plane in 3D, defined by an origin point and two perpendicular X and Y directions.

The normal direction  of the plane is then defined as
the cross product of its X and Y directions.
-}
data Plane3d coordinateSystem defines where
  Plane3d ::
    Point3d (space @ units) ->
    PlanarBasis3d space defines ->
    Plane3d (space @ units) defines

deriving instance Eq (Plane3d (space @ units) defines)

deriving instance Ord (Plane3d (space @ units) defines)

deriving instance Show (Qty units) => Show (Plane3d (space @ units) defines)

instance FFI (Plane3d (space @ Meters) defines) where
  representation = FFI.classRepresentation "Plane3d"

----- Frame3d -----

-- | A frame of reference in 3D, defined by an origin point and orientation.
type Frame3d :: CoordinateSystem -> LocalSpace -> Type
data Frame3d coordinateSystem defines where
  Frame3d :: Point3d (space @ units) -> Basis3d space defines -> Frame3d (space @ units) defines

deriving instance Eq (Frame3d (space @ units) defines)

deriving instance Show (Qty units) => Show (Frame3d (space @ units) defines)

instance FFI (Frame3d (space @ Meters) defines) where
  representation = FFI.classRepresentation "Frame3d"

instance
  (space1 ~ space2, defines1 ~ defines2) =>
  Units.Coercion (Frame3d (space1 @ units1) defines1) (Frame3d (space2 @ units2) defines2)
  where
  coerce (Frame3d p0 b) = Frame3d (Units.coerce p0) b

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

deriving instance Show (Qty units) => Show (Transform3d tag (space @ units))

instance HasUnits (Transform3d tag (space @ units)) units (Transform3d tag (space @ Unitless))

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
  transform1 >> transform2 =
    Transform3d
      (Point3d Qty.zero Qty.zero Qty.zero * transform1 * transform2)
      (Vector3d 1.0 0.0 0.0 * transform1 * transform2)
      (Vector3d 0.0 1.0 0.0 * transform1 * transform2)
      (Vector3d 0.0 0.0 1.0 * transform1 * transform2)
