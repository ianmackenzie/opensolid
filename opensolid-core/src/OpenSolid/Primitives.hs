module OpenSolid.Primitives where

import OpenSolid.Angle qualified as Angle
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Units (Meters, Radians, SquareMeters)
import OpenSolid.Units qualified as Units

----- Vector2d -----

data Vector2d (coordinateSystem :: CoordinateSystem) where
  Vector2d :: Qty units -> Qty units -> Vector2d (space @ units)

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
  coerce (Vector2d vx vy) = Vector2d (Units.coerce vx) (Units.coerce vy)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) units1
  where
  Vector2d x1 y1 ~= Vector2d x2 y2 = Qty.hypot2 (x2 - x1) (y2 - y1) ~= Qty.zero

instance Negation (Vector2d (space @ units)) where
  negate (Vector2d vx vy) = Vector2d (negate vx) (negate vy)

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
  Vector2d x1 y1 .<>. Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Qty units3)
  where
  Vector2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector2d (space1 @ units)) (Direction2d space2) (Qty (units :*: Unitless))
  where
  v .<>. Unit2d d = v .<>. d

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Qty units)
  where
  v <> Unit2d d = v <> d

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (Vector2d (space2 @ units)) (Qty (Unitless :*: units))
  where
  Unit2d d .<>. v = d .<>. v

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Qty units)
  where
  Unit2d d <> v = d <> v

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Qty (units1 :*: units2))
  where
  Vector2d x1 y1 .><. Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication (Vector2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Qty units3)
  where
  Vector2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector2d (space1 @ units)) (Direction2d space2) (Qty (units :*: Unitless))
  where
  v1 .><. Unit2d v2 = v1 .><. v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (Direction2d space2) (Qty units)
  where
  v1 >< Unit2d v2 = v1 >< v2

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (Vector2d (space2 @ units)) (Qty (Unitless :*: units))
  where
  Unit2d v1 .><. v2 = v1 .><. v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Vector2d (space2 @ units)) (Qty units)
  where
  Unit2d v1 >< v2 = v1 >< v2

----- Direction2d -----

{-| A direction in 2D.

This is effectively a type-safe unit vector.
-}
newtype Direction2d (space :: Type) = Unit2d (Vector2d (space @ Unitless))
  deriving (Eq, Show)

instance HasUnits (Direction2d space) Unitless (Direction2d space)

instance FFI (Direction2d space) where
  representation = FFI.classRepresentation "Direction2d"

instance space1 ~ space2 => Units.Coercion (Direction2d space1) (Direction2d space2) where
  coerce = identity

instance
  space1 ~ space2 =>
  ApproximateEquality (Direction2d space1) (Direction2d space2) Radians
  where
  d1 ~= d2 = Angle.atan2 (d1 >< d2) (d1 <> d2) ~= Angle.zero

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

instance
  Multiplication'
    (Qty units)
    (Direction2d space)
    (Vector2d (space @ (units :*: Unitless)))
  where
  scale .*. Unit2d v = scale .*. v

instance Multiplication (Qty units) (Direction2d space) (Vector2d (space @ units)) where
  scale * Unit2d v = scale * v

instance
  Multiplication'
    (Direction2d space)
    (Qty units)
    (Vector2d (space @ (Unitless :*: units)))
  where
  Unit2d v .*. scale = v .*. scale

instance Multiplication (Direction2d space) (Qty units) (Vector2d (space @ units)) where
  Unit2d v * scale = v * scale

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (Direction2d space2) (Qty (Unitless :*: Unitless))
  where
  Unit2d v1 .<>. Unit2d v2 = v1 .<>. v2

instance space1 ~ space2 => DotMultiplication (Direction2d space1) (Direction2d space2) Float where
  Unit2d v1 <> Unit2d v2 = v1 <> v2

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (Direction2d space2) (Qty (Unitless :*: Unitless))
  where
  Unit2d v1 .><. Unit2d v2 = v1 .><. v2

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Direction2d space2) Float
  where
  Unit2d v1 >< Unit2d v2 = v1 >< v2

----- Basis2d -----

type role Basis2d nominal nominal

type Basis2d :: Type -> LocalSpace -> Type
data Basis2d space defines where
  Basis2d :: Direction2d space -> Direction2d space -> Basis2d space defines

deriving instance Eq (Basis2d space defines)

deriving instance Show (Basis2d space defines)

----- Point2d -----

data Point2d (coordinateSystem :: CoordinateSystem) where
  Point2d :: Qty units -> Qty units -> Point2d (space @ units)

deriving instance Eq (Point2d (space @ units))

deriving instance Ord (Point2d (space @ units))

deriving instance Show (Point2d (space @ units))

instance FFI (Point2d (space @ Meters)) where
  representation = FFI.classRepresentation "Point2d"

instance FFI (Point2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvPoint"

instance HasUnits (Point2d (space @ units)) units (Point2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Point2d (space1 @ unitsA)) (Point2d (space2 @ unitsB))
  where
  coerce (Point2d px py) = Point2d (Units.coerce px) (Units.coerce py)

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
  VectorBounds2d :: Range units -> Range units -> VectorBounds2d (space @ units)

deriving instance Show (VectorBounds2d (space @ units))

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

instance
  Multiplication'
    Sign
    (VectorBounds2d (space @ units))
    (VectorBounds2d (space @ (Unitless :*: units)))
  where
  Positive .*. vectorBounds = Units.coerce vectorBounds
  Negative .*. vectorBounds = Units.coerce -vectorBounds

instance Multiplication Sign (VectorBounds2d (space @ units)) (VectorBounds2d (space @ units)) where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = -vectorBounds

instance
  Multiplication'
    (VectorBounds2d (space @ units))
    Sign
    (VectorBounds2d (space @ (units :*: Unitless)))
  where
  vectorBounds .*. Positive = Units.coerce vectorBounds
  vectorBounds .*. Negative = Units.coerce -vectorBounds

instance Multiplication (VectorBounds2d (space @ units)) Sign (VectorBounds2d (space @ units)) where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = -vectorBounds

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
    (Range units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  range .*. VectorBounds2d x y = VectorBounds2d (range .*. x) (range .*. y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (VectorBounds2d (space @ units2)) (VectorBounds2d (space @ units3))
  where
  range * VectorBounds2d x y = VectorBounds2d (range * x) (range * y)

instance
  Multiplication'
    (VectorBounds2d (space @ units1))
    (Range units2)
    (VectorBounds2d (space @ (units1 :*: units2)))
  where
  VectorBounds2d x y .*. range = VectorBounds2d (x .*. range) (y .*. range)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorBounds2d (space @ units1)) (Range units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * range = VectorBounds2d (x * range) (y * range)

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
    (Range units2)
    (VectorBounds2d (space @ (units1 :/: units2)))
  where
  VectorBounds2d x y ./. range = VectorBounds2d (x ./. range) (y ./. range)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorBounds2d (space @ units1)) (Range units2) (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y / range = VectorBounds2d (x / range) (y / range)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (Vector2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)
  where
  Vector2d x1 y1 <> VectorBounds2d x2 y2 = x1 * x2 + y1 * y2

instance
  space ~ space_ =>
  DotMultiplication'
    (Vector2d (space @ units1))
    (VectorBounds2d (space_ @ units2))
    (Range (units1 :*: units2))
  where
  Vector2d x1 y1 .<>. VectorBounds2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication (VectorBounds2d (space @ units1)) (Vector2d (space_ @ units2)) (Range units3)
  where
  VectorBounds2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorBounds2d (space @ units1))
    (Vector2d (space_ @ units2))
    (Range (units1 :*: units2))
  where
  VectorBounds2d x1 y1 .<>. Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  space ~ space_ =>
  DotMultiplication (Direction2d space) (VectorBounds2d (space_ @ units)) (Range units)
  where
  Unit2d vector <> vectorBounds = vector <> vectorBounds

instance
  space ~ space_ =>
  DotMultiplication'
    (Direction2d space)
    (VectorBounds2d (space_ @ units))
    (Range (Unitless :*: units))
  where
  Unit2d vector .<>. vectorBounds = vector .<>. vectorBounds

instance
  space ~ space_ =>
  DotMultiplication (VectorBounds2d (space @ units)) (Direction2d space_) (Range units)
  where
  vectorBounds <> Unit2d vector = vectorBounds <> vector

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorBounds2d (space @ units))
    (Direction2d space_)
    (Range (units :*: Unitless))
  where
  vectorBounds .<>. Unit2d vector = vectorBounds .<>. vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (VectorBounds2d (space @ units1))
    (VectorBounds2d (space_ @ units2))
    (Range units3)
  where
  VectorBounds2d x1 y1 <> VectorBounds2d x2 y2 = x1 * x2 + y1 * y2

instance
  space ~ space_ =>
  DotMultiplication'
    (VectorBounds2d (space @ units1))
    (VectorBounds2d (space_ @ units2))
    (Range (units1 :*: units2))
  where
  VectorBounds2d x1 y1 .<>. VectorBounds2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication (Vector2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)
  where
  Vector2d x1 y1 >< VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

instance
  space ~ space_ =>
  CrossMultiplication'
    (Vector2d (space @ units1))
    (VectorBounds2d (space_ @ units2))
    (Range (units1 :*: units2))
  where
  Vector2d x1 y1 .><. VectorBounds2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (VectorBounds2d (space @ units1))
    (Vector2d (space_ @ units2))
    (Range units3)
  where
  VectorBounds2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorBounds2d (space @ units1))
    (Vector2d (space_ @ units2))
    (Range (units1 :*: units2))
  where
  VectorBounds2d x1 y1 .><. Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  space ~ space_ =>
  CrossMultiplication (Direction2d space) (VectorBounds2d (space_ @ units)) (Range units)
  where
  Unit2d vector >< vectorBounds = vector >< vectorBounds

instance
  space ~ space_ =>
  CrossMultiplication'
    (Direction2d space)
    (VectorBounds2d (space_ @ units))
    (Range (Unitless :*: units))
  where
  Unit2d vector .><. vectorBounds = vector .><. vectorBounds

instance
  space ~ space_ =>
  CrossMultiplication (VectorBounds2d (space @ units)) (Direction2d space_) (Range units)
  where
  vectorBounds >< Unit2d vector = vectorBounds >< vector

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorBounds2d (space @ units))
    (Direction2d space_)
    (Range (units :*: Unitless))
  where
  vectorBounds .><. Unit2d vector = vectorBounds .><. vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication (VectorBounds2d (space @ units1)) (VectorBounds2d (space_ @ units2)) (Range units3)
  where
  VectorBounds2d x1 y1 >< VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

instance
  space ~ space_ =>
  CrossMultiplication'
    (VectorBounds2d (space @ units1))
    (VectorBounds2d (space_ @ units2))
    (Range (units1 :*: units2))
  where
  VectorBounds2d x1 y1 .><. VectorBounds2d x2 y2 = x1 .*. y2 - y1 .*. x2

----- Bounds2d -----

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d ::
    Range units ->
    Range units ->
    Bounds2d (space @ units)

deriving instance Show (Bounds2d (space @ units))

instance HasUnits (Bounds2d (space @ units)) units (Bounds2d (space @ Unitless))

class Bounded2d a (coordinateSystem :: CoordinateSystem) | a -> coordinateSystem where
  bounds :: a -> Bounds2d coordinateSystem

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds2d (space1 @ unitsA)) (Bounds2d (space2 @ unitsB))
  where
  coerce (Bounds2d x y) = Bounds2d (Units.coerce x) (Units.coerce y)

instance Bounded2d (Point2d (space @ units)) (space @ units) where
  bounds (Point2d px py) = Bounds2d (Range.constant px) (Range.constant py)

instance Bounded2d (Bounds2d (space @ units)) (space @ units) where
  bounds = identity

instance FFI (Bounds2d (space @ Meters)) where
  representation = FFI.classRepresentation "Bounds2d"

instance FFI (Bounds2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvBounds"

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Point2d px py - Bounds2d bx by = VectorBounds2d (px - bx) (py - by)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Bounds2d bx by - Point2d px py = VectorBounds2d (bx - px) (by - py)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - Bounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 + Vector2d x2 y2 = Bounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 + VectorBounds2d x2 y2 = Bounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - Vector2d x2 y2 = Bounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - VectorBounds2d x2 y2 = Bounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Point2d px py ~= Bounds2d bx by = px ~= bx && py ~= by

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  box ~= point = point ~= box

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Point2d px py ^ Bounds2d bx by = px ^ bx && py ^ by

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  box ^ point = point ^ box

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Bounds2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Bounds2d x1 y1 ^ Bounds2d x2 y2 = x1 ^ x2 && y1 ^ y2

----- Axis2d -----

data Axis2d (coordinateSystem :: CoordinateSystem) where
  Axis2d ::
    Point2d (space @ units) ->
    Direction2d space ->
    Axis2d (space @ units)

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Axis2d (space @ units))

----- Frame2d -----

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    Point2d (space @ units) ->
    Basis2d space defines ->
    Frame2d (space @ units) defines

deriving instance Eq (Frame2d (space @ units) defines)

deriving instance Show (Frame2d (space @ units) defines)

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

deriving instance Show (Transform2d tag (space @ units))

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
  ( Composition tag1 tag2 tag3
  , space1 ~ space2
  , units1 ~ units2
  ) =>
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
