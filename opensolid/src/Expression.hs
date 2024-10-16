module Expression
  ( Expression
  , zero
  , origin
  , constant
  , xy
  , xyz
  , xComponent
  , yComponent
  , zComponent
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , parameter
  , u
  , v
  , sqrt
  , sqrt'
  , squared
  , squared'
  , sin
  , cos
  , CurveDerivative
  , curveDerivative
  , SurfaceDerivative
  , surfaceDerivative
  , ValueFunction
  , valueFunction
  , BoundsFunction
  , boundsFunction
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Direction2d (Direction2d)
import Expression.Expression1d (Expression1d)
import Expression.Expression1d qualified as Expression1d
import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import IO qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Point3d (Point3d (Point3d))
import Point3d qualified
import Qty qualified
import Range (Range (Range))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Units qualified
import Uv qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import Prelude (Double)

type role Expression nominal nominal

data Expression input output where
  Curve1d ::
    Expression1d Float ->
    Expression Float (Qty units)
  Surface1d ::
    Expression1d Uv.Point ->
    Expression Uv.Point (Qty units)
  Curve2d ::
    Expression1d Float ->
    Expression1d Float ->
    Expression Float (Point2d (space @ units))
  Surface2d ::
    Expression1d Uv.Point ->
    Expression1d Uv.Point ->
    Expression Uv.Point (Point2d (space @ units))
  VectorCurve2d ::
    Expression1d Float ->
    Expression1d Float ->
    Expression Float (Vector2d (space @ units))
  VectorSurface2d ::
    Expression1d Uv.Point ->
    Expression1d Uv.Point ->
    Expression Uv.Point (Vector2d (space @ units))
  Curve3d ::
    Expression1d Float ->
    Expression1d Float ->
    Expression1d Float ->
    Expression Float (Point3d (space @ units))
  Surface3d ::
    Expression1d Uv.Point ->
    Expression1d Uv.Point ->
    Expression1d Uv.Point ->
    Expression Uv.Point (Point3d (space @ units))
  VectorCurve3d ::
    Expression1d Float ->
    Expression1d Float ->
    Expression1d Float ->
    Expression Float (Vector3d (space @ units))
  VectorSurface3d ::
    Expression1d Uv.Point ->
    Expression1d Uv.Point ->
    Expression1d Uv.Point ->
    Expression Uv.Point (Vector3d (space @ units))

expression1d :: Qty units -> Expression1d input
expression1d value = Expression1d.constant (Units.coerce value)

curve1d :: Qty units -> Expression Float (Qty units)
curve1d x = Curve1d (expression1d x)

surface1d :: Qty units -> Expression Uv.Point (Qty units)
surface1d x = Surface1d (expression1d x)

curve2d :: Point2d (space @ units) -> Expression Float (Point2d (space @ units))
curve2d (Point2d x y) = Curve2d (expression1d x) (expression1d y)

surface2d :: Point2d (space @ units) -> Expression Uv.Point (Point2d (space @ units))
surface2d (Point2d x y) = Surface2d (expression1d x) (expression1d y)

vectorCurve2d :: Vector2d (space @ units) -> Expression Float (Vector2d (space @ units))
vectorCurve2d (Vector2d x y) = VectorCurve2d (expression1d x) (expression1d y)

vectorSurface2d :: Vector2d (space @ units) -> Expression Uv.Point (Vector2d (space @ units))
vectorSurface2d (Vector2d x y) = VectorSurface2d (expression1d x) (expression1d y)

curve3d :: Point3d (space @ units) -> Expression Float (Point3d (space @ units))
curve3d (Point3d x y z) = Curve3d (expression1d x) (expression1d y) (expression1d z)

surface3d :: Point3d (space @ units) -> Expression Uv.Point (Point3d (space @ units))
surface3d (Point3d x y z) = Surface3d (expression1d x) (expression1d y) (expression1d z)

vectorCurve3d :: Vector3d (space @ units) -> Expression Float (Vector3d (space @ units))
vectorCurve3d (Vector3d x y z) = VectorCurve3d (expression1d x) (expression1d y) (expression1d z)

vectorSurface3d :: Vector3d (space @ units) -> Expression Uv.Point (Vector3d (space @ units))
vectorSurface3d (Vector3d x y z) = VectorSurface3d (expression1d x) (expression1d y) (expression1d z)

-------------
--- UNITS ---
-------------

instance HasUnits (Expression input (Qty units)) where
  type UnitsOf (Expression input (Qty units)) = units

instance HasUnits (Expression input (Vector2d (space @ units))) where
  type UnitsOf (Expression input (Vector2d (space @ units))) = units

instance HasUnits (Expression input (Vector3d (space @ units))) where
  type UnitsOf (Expression input (Vector3d (space @ units))) = units

instance HasUnits (Expression input (Point2d (space @ units))) where
  type UnitsOf (Expression input (Point2d (space @ units))) = units

instance HasUnits (Expression input (Point3d (space @ units))) where
  type UnitsOf (Expression input (Point3d (space @ units))) = units

instance
  input1 ~ input2 =>
  Units.Coercion (Expression input1 (Qty units1)) (Expression input2 (Qty units2))
  where
  coerce (Curve1d expression) = Curve1d expression
  coerce (Surface1d expression) = Surface1d expression

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
  where
  coerce (VectorCurve2d x y) = VectorCurve2d x y
  coerce (VectorSurface2d x y) = VectorSurface2d x y

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Vector3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
  where
  coerce (VectorCurve3d x y z) = VectorCurve3d x y z
  coerce (VectorSurface3d x y z) = VectorSurface3d x y z

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Point2d (space2 @ units2)))
  where
  coerce (Curve2d x y) = Curve2d x y
  coerce (Surface2d x y) = Surface2d x y

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Expression input (Point3d (space1 @ units1)))
    (Expression input (Point3d (space2 @ units2)))
  where
  coerce (Curve3d x y z) = Curve3d x y z
  coerce (Surface3d x y z) = Surface3d x y z

----------------
--- NEGATION ---
----------------

instance Negation (Expression input (Qty units)) where
  negate (Curve1d x) = Curve1d (Expression1d.negated x)
  negate (Surface1d x) = Surface1d (Expression1d.negated x)

instance Negation (Expression input (Vector2d (space @ units))) where
  negate (VectorCurve2d x y) = VectorCurve2d (Expression1d.negated x) (Expression1d.negated y)
  negate (VectorSurface2d x y) = VectorSurface2d (Expression1d.negated x) (Expression1d.negated y)

instance Negation (Expression input (Vector3d (space @ units))) where
  negate (VectorCurve3d x y z) =
    VectorCurve3d (Expression1d.negated x) (Expression1d.negated y) (Expression1d.negated z)
  negate (VectorSurface3d x y z) =
    VectorSurface3d (Expression1d.negated x) (Expression1d.negated y) (Expression1d.negated z)

instance Multiplication' Sign (Expression input (Qty units)) where
  type Sign .*. Expression input (Qty units) = Expression input (Qty (Unitless :*: units))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Expression input (Qty units)) Sign where
  type Expression input (Qty units) .*. Sign = Expression input (Qty (units :*: Unitless))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' Sign (Expression input (Vector2d (space @ units))) where
  type
    Sign .*. Expression input (Vector2d (space @ units)) =
      Expression input (Vector2d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Expression input (Vector2d (space @ units))) Sign where
  type
    Expression input (Vector2d (space @ units)) .*. Sign =
      Expression input (Vector2d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' Sign (Expression input (Vector3d (space @ units))) where
  type
    Sign .*. Expression input (Vector3d (space @ units)) =
      Expression input (Vector3d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Expression input (Vector3d (space @ units))) Sign where
  type
    Expression input (Vector3d (space @ units)) .*. Sign =
      Expression input (Vector3d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication Sign (Expression input (Qty units)) (Expression input (Qty units))

instance Multiplication (Expression input (Qty units)) Sign (Expression input (Qty units))

instance
  Multiplication
    Sign
    (Expression input (Vector2d (space @ units)))
    (Expression input (Vector2d (space @ units)))

instance
  Multiplication
    (Expression input (Vector2d (space @ units)))
    Sign
    (Expression input (Vector2d (space @ units)))

instance
  Multiplication
    Sign
    (Expression input (Vector3d (space @ units)))
    (Expression input (Vector3d (space @ units)))

instance
  Multiplication
    (Expression input (Vector3d (space @ units)))
    Sign
    (Expression input (Vector3d (space @ units)))

----------------
--- ADDITION ---
----------------

instance
  units1 ~ units2 =>
  Addition
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units1))
  where
  Curve1d lhs + Curve1d rhs = Curve1d (Expression1d.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition
    (Expression Uv.Point (Qty units1))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Qty units1))
  where
  Surface1d lhs + Surface1d rhs = Surface1d (Expression1d.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition (Expression Float (Qty units1)) (Qty units2) (Expression Float (Qty units1))
  where
  expression + value = expression + curve1d value

instance
  units1 ~ units2 =>
  Addition (Expression Uv.Point (Qty units1)) (Qty units2) (Expression Uv.Point (Qty units1))
  where
  expression + value = expression + surface1d value

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Expression Float (Qty units2)) (Expression Float (Qty units1))
  where
  value + expression = curve1d value + expression

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Expression Uv.Point (Qty units2)) (Expression Uv.Point (Qty units1))
  where
  value + expression = surface1d value + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d x1 y1 + VectorCurve2d x2 y2 =
    VectorCurve2d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  VectorSurface2d x1 y1 + VectorSurface2d x2 y2 =
    VectorSurface2d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + vectorCurve2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + vectorSurface2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  vector + expression = vectorCurve2d vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  vector + expression = vectorSurface2d vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d x1 y1 z1 + VectorCurve3d x2 y2 z2 =
    VectorCurve3d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2) (Expression1d.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Vector3d (space1 @ units1)))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  VectorSurface3d x1 y1 z1 + VectorSurface3d x2 y2 z2 =
    VectorSurface3d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2) (Expression1d.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + vectorCurve3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + vectorSurface3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  vector + expression = vectorCurve3d vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  vector + expression = vectorSurface3d vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  Curve2d x1 y1 + VectorCurve2d x2 y2 =
    Curve2d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Point2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Point2d (space1 @ units1)))
  where
  Surface2d x1 y1 + VectorSurface2d x2 y2 =
    Surface2d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Float (Point2d (space1 @ units1)))
  where
  expression + vector = expression + vectorCurve2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Uv.Point (Point2d (space1 @ units1)))
  where
  expression + vector = expression + vectorSurface2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  point + expression = curve2d point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Point2d (space1 @ units1)))
  where
  point + expression = surface2d point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  Curve3d x1 y1 z1 + VectorCurve3d x2 y2 z2 =
    Curve3d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2) (Expression1d.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Point3d (space1 @ units1)))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Point3d (space1 @ units1)))
  where
  Surface3d x1 y1 z1 + VectorSurface3d x2 y2 z2 =
    Surface3d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2) (Expression1d.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Float (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Float (Point3d (space1 @ units1)))
  where
  expression + vector = expression + vectorCurve3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression Uv.Point (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Uv.Point (Point3d (space1 @ units1)))
  where
  expression + vector = expression + vectorSurface3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  point + expression = curve3d point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Point3d (space1 @ units1)))
  where
  point + expression = surface3d point + expression

-------------------
--- SUBTRACTION ---
-------------------

instance
  units1 ~ units2 =>
  Subtraction
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units1))
  where
  Curve1d lhs - Curve1d rhs = Curve1d (Expression1d.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction
    (Expression Uv.Point (Qty units1))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Qty units1))
  where
  Surface1d lhs - Surface1d rhs = Surface1d (Expression1d.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction (Expression Float (Qty units1)) (Qty units2) (Expression Float (Qty units1))
  where
  expression - value = expression - curve1d value

instance
  units1 ~ units2 =>
  Subtraction (Expression Uv.Point (Qty units1)) (Qty units2) (Expression Uv.Point (Qty units1))
  where
  expression - value = expression - surface1d value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Expression Float (Qty units2)) (Expression Float (Qty units1))
  where
  value - expression = curve1d value - expression

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Expression Uv.Point (Qty units2)) (Expression Uv.Point (Qty units1))
  where
  value - expression = surface1d value - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d x1 y1 - VectorCurve2d x2 y2 =
    VectorCurve2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  VectorSurface2d x1 y1 - VectorSurface2d x2 y2 =
    VectorSurface2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - vectorCurve2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - vectorSurface2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  vector - expression = vectorCurve2d vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  vector - expression = vectorSurface2d vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d x1 y1 z1 - VectorCurve3d x2 y2 z2 =
    VectorCurve3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Vector3d (space1 @ units1)))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  VectorSurface3d x1 y1 z1 - VectorSurface3d x2 y2 z2 =
    VectorSurface3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - vectorCurve3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - vectorSurface3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  vector - expression = vectorCurve3d vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  vector - expression = vectorSurface3d vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  Curve2d x1 y1 - VectorCurve2d x2 y2 =
    Curve2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Point2d (space1 @ units1)))
  where
  Surface2d x1 y1 - VectorSurface2d x2 y2 =
    Surface2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Float (Point2d (space1 @ units1)))
  where
  expression - vector = expression - vectorCurve2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Uv.Point (Point2d (space1 @ units1)))
  where
  expression - vector = expression - vectorSurface2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Point2d (space1 @ units1)))
  where
  point - expression = curve2d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Point2d (space1 @ units1)))
  where
  point - expression = surface2d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Expression Float (Point2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  Curve2d x1 y1 - Curve2d x2 y2 =
    VectorCurve2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point2d (space1 @ units1)))
    (Expression Uv.Point (Point2d (space2 @ units2)))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  Surface2d x1 y1 - Surface2d x2 y2 =
    VectorSurface2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  expression - point = expression - curve2d point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  expression - point = expression - surface2d point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression Float (Point2d (space2 @ units2)))
    (Expression Float (Vector2d (space1 @ units1)))
  where
  point - expression = curve2d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression Uv.Point (Point2d (space2 @ units2)))
    (Expression Uv.Point (Vector2d (space1 @ units1)))
  where
  point - expression = surface2d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  Curve3d x1 y1 z1 - VectorCurve3d x2 y2 z2 =
    Curve3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point3d (space1 @ units1)))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Point3d (space1 @ units1)))
  where
  Surface3d x1 y1 z1 - VectorSurface3d x2 y2 z2 =
    Surface3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Float (Point3d (space1 @ units1)))
  where
  expression - vector = expression - vectorCurve3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression Uv.Point (Point3d (space1 @ units1)))
  where
  expression - vector = expression - vectorSurface3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression Float (Vector3d (space2 @ units2)))
    (Expression Float (Point3d (space1 @ units1)))
  where
  point - expression = curve3d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression Uv.Point (Vector3d (space2 @ units2)))
    (Expression Uv.Point (Point3d (space1 @ units1)))
  where
  point - expression = surface3d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Expression Float (Point3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  Curve3d x1 y1 z1 - Curve3d x2 y2 z2 =
    VectorCurve3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point3d (space1 @ units1)))
    (Expression Uv.Point (Point3d (space2 @ units2)))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  Surface3d x1 y1 z1 - Surface3d x2 y2 z2 =
    VectorSurface3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Float (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  expression - point = expression - curve3d point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression Uv.Point (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  expression - point = expression - surface3d point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression Float (Point3d (space2 @ units2)))
    (Expression Float (Vector3d (space1 @ units1)))
  where
  point - expression = curve3d point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression Uv.Point (Point3d (space2 @ units2)))
    (Expression Uv.Point (Vector3d (space1 @ units1)))
  where
  point - expression = surface3d point - expression

----------------------
--- MULTIPLICATION ---
----------------------

--------------------------------
--- Multiplication instances ---
--------------------------------

--- Qty-Qty ---
---------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Uv.Point (Qty units1))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Expression Float (Qty units2)) (Expression Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Expression Uv.Point (Qty units2)) (Expression Uv.Point (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Expression Float (Qty units1)) (Qty units2) (Expression Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Expression Uv.Point (Qty units1)) (Qty units2) (Expression Uv.Point (Qty units3))

--- Qty-Vector2d ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Qty units1))
    (Expression Float (Vector2d (space @ units2)))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Uv.Point (Qty units1))
    (Expression Uv.Point (Vector2d (space @ units2)))
    (Expression Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Expression Float (Vector2d (space @ units2)))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Expression Uv.Point (Vector2d (space @ units2)))
    (Expression Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Qty units1))
    (Vector2d (space @ units2))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Uv.Point (Qty units1))
    (Vector2d (space @ units2))
    (Expression Uv.Point (Vector2d (space @ units3)))

-- Qty-Direction2d --
---------------------

instance
  Multiplication
    (Expression Float (Qty units))
    (Direction2d space)
    (Expression Float (Vector2d (space @ units)))

instance
  Multiplication
    (Expression Uv.Point (Qty units))
    (Direction2d space)
    (Expression Uv.Point (Vector2d (space @ units)))

--- Vector2d-Qty ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Uv.Point (Vector2d (space @ units1)))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Float (Vector2d (space @ units1)))
    (Qty units2)
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression Uv.Point (Vector2d (space @ units1)))
    (Qty units2)
    (Expression Uv.Point (Vector2d (space @ units3)))

--- Direction2d-Qty ---
-----------------------

instance
  Multiplication
    (Direction2d space)
    (Expression Float (Qty units))
    (Expression Float (Vector2d (space @ units)))

instance
  Multiplication
    (Direction2d space)
    (Expression Uv.Point (Qty units))
    (Expression Uv.Point (Vector2d (space @ units)))

---------------------------------
--- Multiplication' instances ---
---------------------------------

--- Qty-Qty ---
---------------

instance Multiplication' (Expression Float (Qty units1)) (Expression Float (Qty units2)) where
  type
    Expression Float (Qty units1) .*. Expression Float (Qty units2) =
      Expression Float (Qty (units1 :*: units2))
  Curve1d lhs .*. Curve1d rhs = Curve1d (Expression1d.product lhs rhs)

instance Multiplication' (Expression Uv.Point (Qty units1)) (Expression Uv.Point (Qty units2)) where
  type
    Expression Uv.Point (Qty units1) .*. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Qty (units1 :*: units2))
  Surface1d lhs .*. Surface1d rhs = Surface1d (Expression1d.product lhs rhs)

instance Multiplication' (Qty units1) (Expression Float (Qty units2)) where
  type Qty units1 .*. Expression Float (Qty units2) = Expression Float (Qty (units1 :*: units2))
  value .*. expression = curve1d value .*. expression

instance Multiplication' (Qty units1) (Expression Uv.Point (Qty units2)) where
  type
    Qty units1 .*. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Qty (units1 :*: units2))
  value .*. expression = surface1d value .*. expression

instance Multiplication' (Expression Float (Qty units1)) (Qty units2) where
  type Expression Float (Qty units1) .*. Qty units2 = Expression Float (Qty (units1 :*: units2))
  expression .*. value = expression .*. curve1d value

instance Multiplication' (Expression Uv.Point (Qty units1)) (Qty units2) where
  type
    Expression Uv.Point (Qty units1) .*. Qty units2 =
      Expression Uv.Point (Qty (units1 :*: units2))
  expression .*. value = expression .*. surface1d value

--- Qty-Vector2d ---
--------------------

instance Multiplication' (Expression Float (Qty units1)) (Expression Float (Vector2d (space @ units2))) where
  type
    Expression Float (Qty units1) .*. Expression Float (Vector2d (space @ units2)) =
      Expression Float (Vector2d (space @ (units1 :*: units2)))
  Curve1d scale .*. VectorCurve2d x y =
    VectorCurve2d (Expression1d.product scale x) (Expression1d.product scale y)

instance Multiplication' (Expression Uv.Point (Qty units1)) (Expression Uv.Point (Vector2d (space @ units2))) where
  type
    Expression Uv.Point (Qty units1) .*. Expression Uv.Point (Vector2d (space @ units2)) =
      Expression Uv.Point (Vector2d (space @ (units1 :*: units2)))
  Surface1d scale .*. VectorSurface2d x y =
    VectorSurface2d (Expression1d.product scale x) (Expression1d.product scale y)

instance Multiplication' (Qty units1) (Expression Float (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Expression Float (Vector2d (space @ units2)) =
      Expression Float (Vector2d (space @ (units1 :*: units2)))
  value .*. expression = curve1d value .*. expression

instance Multiplication' (Qty units1) (Expression Uv.Point (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Expression Uv.Point (Vector2d (space @ units2)) =
      Expression Uv.Point (Vector2d (space @ (units1 :*: units2)))
  value .*. expression = surface1d value .*. expression

instance Multiplication' (Expression Float (Qty units1)) (Vector2d (space @ units2)) where
  type
    Expression Float (Qty units1) .*. Vector2d (space @ units2) =
      Expression Float (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. vectorCurve2d vector

instance Multiplication' (Expression Uv.Point (Qty units1)) (Vector2d (space @ units2)) where
  type
    Expression Uv.Point (Qty units1) .*. Vector2d (space @ units2) =
      Expression Uv.Point (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. vectorSurface2d vector

--- Qty-Direction2d ---
-----------------------

instance Multiplication' (Expression Float (Qty units)) (Direction2d space) where
  type
    Expression Float (Qty units) .*. Direction2d space =
      Expression Float (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

instance Multiplication' (Expression Uv.Point (Qty units)) (Direction2d space) where
  type
    Expression Uv.Point (Qty units) .*. Direction2d space =
      Expression Uv.Point (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

--- Vector2d-Qty ---
--------------------

instance Multiplication' (Expression Float (Vector2d (space @ units1))) (Expression Float (Qty units2)) where
  type
    Expression Float (Vector2d (space @ units1)) .*. Expression Float (Qty units2) =
      Expression Float (Vector2d (space @ (units1 :*: units2)))
  VectorCurve2d x y .*. Curve1d scale =
    VectorCurve2d (Expression1d.product x scale) (Expression1d.product y scale)

instance Multiplication' (Expression Uv.Point (Vector2d (space @ units1))) (Expression Uv.Point (Qty units2)) where
  type
    Expression Uv.Point (Vector2d (space @ units1)) .*. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Vector2d (space @ (units1 :*: units2)))
  VectorSurface2d x y .*. Surface1d scale =
    VectorSurface2d (Expression1d.product x scale) (Expression1d.product y scale)

instance Multiplication' (Vector2d (space @ units1)) (Expression Float (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Expression Float (Qty units2) =
      Expression Float (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = vectorCurve2d vector .*. expression

instance Multiplication' (Vector2d (space @ units1)) (Expression Uv.Point (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = vectorSurface2d vector .*. expression

instance Multiplication' (Expression Float (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression Float (Vector2d (space @ units1)) .*. Qty units2 =
      Expression Float (Vector2d (space @ (units1 :*: units2)))
  expression .*. value = expression .*. curve1d value

instance Multiplication' (Expression Uv.Point (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression Uv.Point (Vector2d (space @ units1)) .*. Qty units2 =
      Expression Uv.Point (Vector2d (space @ (units1 :*: units2)))
  expression .*. value = expression .*. surface1d value

--- Direction2d-Qty ---
-----------------------

instance Multiplication' (Direction2d space) (Expression Float (Qty units)) where
  type
    Direction2d space .*. Expression Float (Qty units) =
      Expression Float (Vector2d (space @ (Unitless :*: units)))
  direction .*. scale = Vector2d.unit direction .*. scale

instance Multiplication' (Direction2d space) (Expression Uv.Point (Qty units)) where
  type
    Direction2d space .*. Expression Uv.Point (Qty units) =
      Expression Uv.Point (Vector2d (space @ (Unitless :*: units)))
  direction .*. scale = Vector2d.unit direction .*. scale

----------------
--- DIVISION ---
----------------

--- Division instances ---
--------------------------

--- Qty-Qty ---
---------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Float (Qty units1))
    (Expression Float (Qty units2))
    (Expression Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Uv.Point (Qty units1))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Expression Float (Qty units1)) (Qty units2) (Expression Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Expression Uv.Point (Qty units1)) (Qty units2) (Expression Uv.Point (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Expression Float (Qty units2)) (Expression Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Expression Uv.Point (Qty units2)) (Expression Uv.Point (Qty units3))

--- Vector2d-Qty ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Float (Vector2d (space @ units1)))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Uv.Point (Vector2d (space @ units1)))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Expression Float (Qty units2))
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Expression Uv.Point (Qty units2))
    (Expression Uv.Point (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Float (Vector2d (space @ units1)))
    (Qty units2)
    (Expression Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression Uv.Point (Vector2d (space @ units1)))
    (Qty units2)
    (Expression Uv.Point (Vector2d (space @ units3)))

---------------------------
--- Division' instances ---
---------------------------

--- Qty-Qty ---
---------------

instance Division' (Expression Float (Qty units1)) (Expression Float (Qty units2)) where
  type
    Expression Float (Qty units1) ./. Expression Float (Qty units2) =
      Expression Float (Qty (units1 :/: units2))
  Curve1d lhs ./. Curve1d rhs = Curve1d (Expression1d.quotient lhs rhs)

instance Division' (Expression Uv.Point (Qty units1)) (Expression Uv.Point (Qty units2)) where
  type
    Expression Uv.Point (Qty units1) ./. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Qty (units1 :/: units2))
  Surface1d lhs ./. Surface1d rhs = Surface1d (Expression1d.quotient lhs rhs)

instance Division' (Expression Float (Qty units1)) (Qty units2) where
  type Expression Float (Qty units1) ./. Qty units2 = Expression Float (Qty (units1 :/: units2))
  expression ./. value = expression ./. curve1d value

instance Division' (Expression Uv.Point (Qty units1)) (Qty units2) where
  type
    Expression Uv.Point (Qty units1) ./. Qty units2 =
      Expression Uv.Point (Qty (units1 :/: units2))
  expression ./. value = expression ./. surface1d value

instance Division' (Qty units1) (Expression Float (Qty units2)) where
  type Qty units1 ./. Expression Float (Qty units2) = Expression Float (Qty (units1 :/: units2))
  value ./. expression = curve1d value ./. expression

instance Division' (Qty units1) (Expression Uv.Point (Qty units2)) where
  type
    Qty units1 ./. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Qty (units1 :/: units2))
  value ./. expression = surface1d value ./. expression

--- Vector2d-Qty ---
--------------------

instance Division' (Expression Float (Vector2d (space @ units1))) (Expression Float (Qty units2)) where
  type
    Expression Float (Vector2d (space @ units1)) ./. Expression Float (Qty units2) =
      Expression Float (Vector2d (space @ (units1 :/: units2)))
  VectorCurve2d x y ./. Curve1d scale =
    VectorCurve2d (Expression1d.quotient x scale) (Expression1d.quotient y scale)

instance Division' (Expression Uv.Point (Vector2d (space @ units1))) (Expression Uv.Point (Qty units2)) where
  type
    Expression Uv.Point (Vector2d (space @ units1)) ./. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Vector2d (space @ (units1 :/: units2)))
  VectorSurface2d x y ./. Surface1d scale =
    VectorSurface2d (Expression1d.quotient x scale) (Expression1d.quotient y scale)

instance Division' (Vector2d (space @ units1)) (Expression Float (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Expression Float (Qty units2) =
      Expression Float (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = vectorCurve2d vector ./. expression

instance Division' (Vector2d (space @ units1)) (Expression Uv.Point (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Expression Uv.Point (Qty units2) =
      Expression Uv.Point (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = vectorSurface2d vector ./. expression

instance Division' (Expression Float (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression Float (Vector2d (space @ units1)) ./. Qty units2 =
      Expression Float (Vector2d (space @ (units1 :/: units2)))
  expression ./. value = expression ./. curve1d value

instance Division' (Expression Uv.Point (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression Uv.Point (Vector2d (space @ units1)) ./. Qty units2 =
      Expression Uv.Point (Vector2d (space @ (units1 :/: units2)))
  expression ./. value = expression ./. surface1d value

-------------------
--- DOT PRODUCT ---
-------------------

--- DotMultiplication instances ---
-----------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Uv.Point (Qty units3))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Expression Float (Vector2d (space2 @ units)))
    (Expression Float (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Expression Uv.Point (Vector2d (space2 @ units)))
    (Expression Uv.Point (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Expression Float (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Expression Float (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Expression Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Expression Uv.Point (Qty units))

--- DotMultiplication' instances ---
------------------------------------

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
  where
  type
    Expression Float (Vector2d (space1 @ units1))
      .<>. Expression Float (Vector2d (space2 @ units2)) =
      Expression Float (Qty (units1 :*: units2))
  VectorCurve2d x1 y1 .<>. VectorCurve2d x2 y2 =
    Curve1d (Expression1d.sum (Expression1d.product x1 x2) (Expression1d.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Expression Uv.Point (Vector2d (space1 @ units1))
      .<>. Expression Uv.Point (Vector2d (space2 @ units2)) =
      Expression Uv.Point (Qty (units1 :*: units2))
  VectorSurface2d x1 y1 .<>. VectorSurface2d x2 y2 =
    Surface1d (Expression1d.sum (Expression1d.product x1 x2) (Expression1d.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression Float (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Expression Float (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. vectorCurve2d vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression Uv.Point (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Expression Uv.Point (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. vectorSurface2d vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Expression Float (Vector2d (space2 @ units2)) =
      Expression Float (Qty (units1 :*: units2))
  vector .<>. expression = vectorCurve2d vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Expression Uv.Point (Vector2d (space2 @ units2)) =
      Expression Uv.Point (Qty (units1 :*: units2))
  vector .<>. expression = vectorSurface2d vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Float (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Expression Float (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Expression Float (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Expression Uv.Point (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Expression Uv.Point (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Expression Float (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Expression Float (Vector2d (space2 @ units)) =
      Expression Float (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Expression Uv.Point (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Expression Uv.Point (Vector2d (space2 @ units)) =
      Expression Uv.Point (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

---------------------
--- CROSS PRODUCT ---
---------------------

--- CrossMultiplication instances ---
-------------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
    (Expression Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
    (Expression Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression Uv.Point (Qty units3))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Expression Float (Vector2d (space2 @ units)))
    (Expression Float (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Expression Uv.Point (Vector2d (space2 @ units)))
    (Expression Uv.Point (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Expression Float (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Expression Float (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Expression Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Expression Uv.Point (Qty units))

--- CrossMultiplication' instances ---
--------------------------------------

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Float (Vector2d (space1 @ units1)))
    (Expression Float (Vector2d (space2 @ units2)))
  where
  type
    Expression Float (Vector2d (space1 @ units1))
      .><. Expression Float (Vector2d (space2 @ units2)) =
      Expression Float (Qty (units1 :*: units2))
  VectorCurve2d x1 y1 .><. VectorCurve2d x2 y2 =
    Curve1d (Expression1d.difference (Expression1d.product x1 y2) (Expression1d.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Expression Uv.Point (Vector2d (space1 @ units1))
      .><. Expression Uv.Point (Vector2d (space2 @ units2)) =
      Expression Uv.Point (Qty (units1 :*: units2))
  VectorSurface2d x1 y1 .><. VectorSurface2d x2 y2 =
    Surface1d (Expression1d.difference (Expression1d.product x1 y2) (Expression1d.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression Float (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Expression Float (Qty (units1 :*: units2))
  expression .><. vector = expression .><. vectorCurve2d vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression Uv.Point (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Expression Uv.Point (Qty (units1 :*: units2))
  expression .><. vector = expression .><. vectorSurface2d vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Expression Float (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Expression Float (Vector2d (space2 @ units2)) =
      Expression Float (Qty (units1 :*: units2))
  vector .><. expression = vectorCurve2d vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Expression Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Expression Uv.Point (Vector2d (space2 @ units2)) =
      Expression Uv.Point (Qty (units1 :*: units2))
  vector .><. expression = vectorSurface2d vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Float (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Expression Float (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Expression Float (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Expression Uv.Point (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Expression Uv.Point (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Expression Float (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Expression Float (Vector2d (space2 @ units)) =
      Expression Float (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Expression Uv.Point (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Expression Uv.Point (Vector2d (space2 @ units)) =
      Expression Uv.Point (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Expression Float Float)
    (Expression Float output)
    (Expression Float output)
  where
  curve . Curve1d input =
    case curve of
      Curve1d x -> Curve1d (x . input)
      Curve2d x y -> Curve2d (x . input) (y . input)
      VectorCurve2d x y -> VectorCurve2d (x . input) (y . input)
      Curve3d x y z -> Curve3d (x . input) (y . input) (z . input)
      VectorCurve3d x y z -> VectorCurve3d (x . input) (y . input) (z . input)

instance
  Composition
    (Expression Uv.Point Float)
    (Expression Float output)
    (Expression Uv.Point output)
  where
  curve . Surface1d input =
    case curve of
      Curve1d x -> Surface1d (x . input)
      Curve2d x y -> Surface2d (x . input) (y . input)
      VectorCurve2d x y -> VectorSurface2d (x . input) (y . input)
      Curve3d x y z -> Surface3d (x . input) (y . input) (z . input)
      VectorCurve3d x y z -> VectorSurface3d (x . input) (y . input) (z . input)

instance
  Composition
    (Expression Float Uv.Point)
    (Expression Uv.Point output)
    (Expression Float output)
  where
  surface . Curve2d uInput vInput = do
    let inputs = (uInput, vInput)
    case surface of
      Surface1d x -> Curve1d (x . inputs)
      Surface2d x y -> Curve2d (x . inputs) (y . inputs)
      VectorSurface2d x y -> VectorCurve2d (x . inputs) (y . inputs)
      Surface3d x y z -> Curve3d (x . inputs) (y . inputs) (z . inputs)
      VectorSurface3d x y z -> VectorCurve3d (x . inputs) (y . inputs) (z . inputs)

instance
  Composition
    (Expression Uv.Point Uv.Point)
    (Expression Uv.Point output)
    (Expression Uv.Point output)
  where
  surface . Surface2d uInput vInput = do
    let inputs = (uInput, vInput)
    case surface of
      Surface1d x -> Surface1d (x . inputs)
      Surface2d x y -> Surface2d (x . inputs) (y . inputs)
      VectorSurface2d x y -> VectorSurface2d (x . inputs) (y . inputs)
      Surface3d x y z -> Surface3d (x . inputs) (y . inputs) (z . inputs)
      VectorSurface3d x y z -> VectorSurface3d (x . inputs) (y . inputs) (z . inputs)

-----------------
--- FUNCTIONS ---
-----------------

class Zero input output where
  zero :: Expression input output

instance Zero Float (Qty units) where
  zero = constant Qty.zero

instance Zero Uv.Point (Qty units) where
  zero = constant Qty.zero

instance Zero Float (Vector2d (space @ units)) where
  zero = constant Vector2d.zero

instance Zero Uv.Point (Vector2d (space @ units)) where
  zero = constant Vector2d.zero

instance Zero Float (Vector3d (space @ units)) where
  zero = constant Vector3d.zero

instance Zero Uv.Point (Vector3d (space @ units)) where
  zero = constant Vector3d.zero

class Origin input output where
  origin :: Expression input output

instance Origin Float (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Uv.Point (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Float (Point3d (space @ units)) where
  origin = constant Point3d.origin

instance Origin Uv.Point (Point3d (space @ units)) where
  origin = constant Point3d.origin

class Constant input output where
  constant :: output -> Expression input output

instance Constant Float (Qty units) where
  constant value = Curve1d (expression1d value)

instance Constant Uv.Point (Qty units) where
  constant value = Surface1d (expression1d value)

instance Constant Float (Vector2d (space @ units)) where
  constant (Vector2d x y) = VectorCurve2d (expression1d x) (expression1d y)

instance Constant Uv.Point (Vector2d (space @ units)) where
  constant (Vector2d x y) = VectorSurface2d (expression1d x) (expression1d y)

instance Constant Float (Vector3d (space @ units)) where
  constant (Vector3d x y z) = VectorCurve3d (expression1d x) (expression1d y) (expression1d z)

instance Constant Uv.Point (Vector3d (space @ units)) where
  constant (Vector3d x y z) = VectorSurface3d (expression1d x) (expression1d y) (expression1d z)

instance Constant Float (Point2d (space @ units)) where
  constant (Point2d x y) = Curve2d (expression1d x) (expression1d y)

instance Constant Uv.Point (Point2d (space @ units)) where
  constant (Point2d x y) = Surface2d (expression1d x) (expression1d y)

instance Constant Float (Point3d (space @ units)) where
  constant (Point3d x y z) = Curve3d (expression1d x) (expression1d y) (expression1d z)

instance Constant Uv.Point (Point3d (space @ units)) where
  constant (Point3d x y z) = Surface3d (expression1d x) (expression1d y) (expression1d z)

class XY input output where
  xy ::
    Expression input (Qty (UnitsOf output)) ->
    Expression input (Qty (UnitsOf output)) ->
    Expression input output

instance XY Float (Vector2d (space @ units)) where
  xy (Curve1d x) (Curve1d y) = VectorCurve2d x y

instance XY Uv.Point (Vector2d (space @ units)) where
  xy (Surface1d x) (Surface1d y) = VectorSurface2d x y

instance XY Float (Point2d (space @ units)) where
  xy (Curve1d x) (Curve1d y) = Curve2d x y

instance XY Uv.Point (Point2d (space @ units)) where
  xy (Surface1d x) (Surface1d y) = Surface2d x y

class XYZ input output where
  xyz ::
    Expression input (Qty (UnitsOf output)) ->
    Expression input (Qty (UnitsOf output)) ->
    Expression input (Qty (UnitsOf output)) ->
    Expression input output

instance XYZ Float (Vector3d (space @ units)) where
  xyz (Curve1d x) (Curve1d y) (Curve1d z) = VectorCurve3d x y z

instance XYZ Uv.Point (Vector3d (space @ units)) where
  xyz (Surface1d x) (Surface1d y) (Surface1d z) = VectorSurface3d x y z

instance XYZ Float (Point3d (space @ units)) where
  xyz (Curve1d x) (Curve1d y) (Curve1d z) = Curve3d x y z

instance XYZ Uv.Point (Point3d (space @ units)) where
  xyz (Surface1d x) (Surface1d y) (Surface1d z) = Surface3d x y z

class XComponent input output where
  xComponent :: Expression input output -> Expression input (Qty (UnitsOf output))

class YComponent input output where
  yComponent :: Expression input output -> Expression input (Qty (UnitsOf output))

class ZComponent input output where
  zComponent :: Expression input output -> Expression input (Qty (UnitsOf output))

instance XComponent input (Vector2d (space @ units)) where
  xComponent (VectorCurve2d x _) = Curve1d x
  xComponent (VectorSurface2d x _) = Surface1d x

instance XComponent input (Vector3d (space @ units)) where
  xComponent (VectorCurve3d x _ _) = Curve1d x
  xComponent (VectorSurface3d x _ _) = Surface1d x

instance YComponent input (Vector2d (space @ units)) where
  yComponent (VectorCurve2d _ y) = Curve1d y
  yComponent (VectorSurface2d _ y) = Surface1d y

instance YComponent input (Vector3d (space @ units)) where
  yComponent (VectorCurve3d _ y _) = Curve1d y
  yComponent (VectorSurface3d _ y _) = Surface1d y

instance ZComponent input (Vector3d (space @ units)) where
  zComponent (VectorCurve3d _ _ z) = Curve1d z
  zComponent (VectorSurface3d _ _ z) = Surface1d z

class XCoordinate input output where
  xCoordinate :: Expression input output -> Expression input (Qty (UnitsOf output))

class YCoordinate input output where
  yCoordinate :: Expression input output -> Expression input (Qty (UnitsOf output))

class ZCoordinate input output where
  zCoordinate :: Expression input output -> Expression input (Qty (UnitsOf output))

instance XCoordinate input (Point2d (space @ units)) where
  xCoordinate (Curve2d x _) = Curve1d x
  xCoordinate (Surface2d x _) = Surface1d x

instance XCoordinate input (Point3d (space @ units)) where
  xCoordinate (Curve3d x _ _) = Curve1d x
  xCoordinate (Surface3d x _ _) = Surface1d x

instance YCoordinate input (Point2d (space @ units)) where
  yCoordinate (Curve2d _ y) = Curve1d y
  yCoordinate (Surface2d _ y) = Surface1d y

instance YCoordinate input (Point3d (space @ units)) where
  yCoordinate (Curve3d _ y _) = Curve1d y
  yCoordinate (Surface3d _ y _) = Surface1d y

instance ZCoordinate input (Point3d (space @ units)) where
  zCoordinate (Curve3d _ _ z) = Curve1d z
  zCoordinate (Surface3d _ _ z) = Surface1d z

parameter :: Expression Float Float
parameter = Curve1d Expression1d.parameter

u :: Expression Uv.Point Float
u = Surface1d Expression1d.u

v :: Expression Uv.Point Float
v = Surface1d Expression1d.v

squared' :: Expression input (Qty units) -> Expression input (Qty (units :*: units))
squared' (Curve1d expression) = Curve1d (Expression1d.squared expression)
squared' (Surface1d expression) = Surface1d (Expression1d.squared expression)

squared ::
  Units.Squared units1 units2 =>
  Expression input (Qty units1) ->
  Expression input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Expression input (Qty (units :*: units)) -> Expression input (Qty units)
sqrt' (Curve1d expression) = Curve1d (Expression1d.sqrt expression)
sqrt' (Surface1d expression) = Surface1d (Expression1d.sqrt expression)

sqrt ::
  Units.Squared units1 units2 =>
  Expression input (Qty units2) ->
  Expression input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Expression input Angle -> Expression input Float
sin (Curve1d expression) = Curve1d (Expression1d.sin expression)
sin (Surface1d expression) = Surface1d (Expression1d.sin expression)

cos :: Expression input Angle -> Expression input Float
cos (Curve1d expression) = Curve1d (Expression1d.cos expression)
cos (Surface1d expression) = Surface1d (Expression1d.cos expression)

-----------------------
--- DIFFERENTIATION ---
-----------------------

class CurveDerivative expression derivative | expression -> derivative where
  curveDerivative :: expression -> derivative

class SurfaceDerivative expression derivative | expression -> derivative where
  surfaceDerivative :: Uv.Parameter -> expression -> derivative

instance
  CurveDerivative
    (Expression Float (Qty units))
    (Expression Float (Qty units))
  where
  curveDerivative (Curve1d expression) = Curve1d (Expression1d.curveDerivative expression)

instance
  SurfaceDerivative
    (Expression Uv.Point (Qty units))
    (Expression Uv.Point (Qty units))
  where
  surfaceDerivative p (Surface1d expression) = Surface1d (Expression1d.surfaceDerivative p expression)

instance
  CurveDerivative
    (Expression Float (Vector2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  curveDerivative (VectorCurve2d x y) =
    VectorCurve2d
      (Expression1d.curveDerivative x)
      (Expression1d.curveDerivative y)

instance
  CurveDerivative
    (Expression Float (Point2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  curveDerivative (Curve2d x y) =
    VectorCurve2d
      (Expression1d.curveDerivative x)
      (Expression1d.curveDerivative y)

instance
  SurfaceDerivative
    (Expression Uv.Point (Vector2d (space @ units)))
    (Expression Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (VectorSurface2d x y) =
    VectorSurface2d
      (Expression1d.surfaceDerivative p x)
      (Expression1d.surfaceDerivative p y)

instance
  SurfaceDerivative
    (Expression Uv.Point (Point2d (space @ units)))
    (Expression Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (Surface2d x y) =
    VectorSurface2d
      (Expression1d.surfaceDerivative p x)
      (Expression1d.surfaceDerivative p y)

-----------------
--- COMPILING ---
-----------------

type Curve1dValueFunction = Double -> Double

type Curve1dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface1dValueFunction = Double -> Double -> Double

type Surface1dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

type Curve2dValueFunction = Double -> Ptr Double -> IO ()

type Curve2dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface2dValueFunction = Double -> Double -> Ptr Double -> IO ()

type Surface2dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "opensolid_curve1d_value_function"
  opensolid_curve1d_value_function :: Expression1d.Ptr -> FunPtr Curve1dValueFunction

foreign import ccall unsafe "opensolid_curve1d_bounds_function"
  opensolid_curve1d_bounds_function :: Expression1d.Ptr -> FunPtr Curve1dBoundsFunction

foreign import ccall unsafe "opensolid_surface1d_value_function"
  opensolid_surface1d_value_function :: Expression1d.Ptr -> FunPtr Surface1dValueFunction

foreign import ccall unsafe "opensolid_surface1d_bounds_function"
  opensolid_surface1d_bounds_function :: Expression1d.Ptr -> FunPtr Surface1dBoundsFunction

foreign import ccall unsafe "opensolid_curve2d_value_function"
  opensolid_curve2d_value_function :: Expression1d.Ptr -> Expression1d.Ptr -> FunPtr Curve2dValueFunction

foreign import ccall unsafe "opensolid_curve2d_bounds_function"
  opensolid_curve2d_bounds_function :: Expression1d.Ptr -> Expression1d.Ptr -> FunPtr Curve2dBoundsFunction

foreign import ccall unsafe "opensolid_surface2d_value_function"
  opensolid_surface2d_value_function :: Expression1d.Ptr -> Expression1d.Ptr -> FunPtr Surface2dValueFunction

foreign import ccall unsafe "opensolid_surface2d_bounds_function"
  opensolid_surface2d_bounds_function :: Expression1d.Ptr -> Expression1d.Ptr -> FunPtr Surface2dBoundsFunction

foreign import ccall unsafe "dynamic"
  curve1d_value_function :: FunPtr Curve1dValueFunction -> Curve1dValueFunction

foreign import ccall unsafe "dynamic"
  curve1d_bounds_function :: FunPtr Curve1dBoundsFunction -> Curve1dBoundsFunction

foreign import ccall unsafe "dynamic"
  surface1d_value_function :: FunPtr Surface1dValueFunction -> Surface1dValueFunction

foreign import ccall unsafe "dynamic"
  surface1d_bounds_function :: FunPtr Surface1dBoundsFunction -> Surface1dBoundsFunction

foreign import ccall unsafe "dynamic"
  curve2d_value_function :: FunPtr Curve2dValueFunction -> Curve2dValueFunction

foreign import ccall unsafe "dynamic"
  curve2d_bounds_function :: FunPtr Curve2dBoundsFunction -> Curve2dBoundsFunction

foreign import ccall unsafe "dynamic"
  surface2d_value_function :: FunPtr Surface2dValueFunction -> Surface2dValueFunction

foreign import ccall unsafe "dynamic"
  surface2d_bounds_function :: FunPtr Surface2dBoundsFunction -> Surface2dBoundsFunction

-- TODO perform garbage collection on JIT-compiled functions:
-- use GHC.Weak.mkWeak on f# to associate a finalizer with it
-- that calls a Rust function to delete the underlying JIT-compiled function/module

class ValueFunction expression function | expression -> function, function -> expression where
  valueFunction :: expression -> function

class BoundsFunction expression function | expression -> function, function -> expression where
  boundsFunction :: expression -> function

instance ValueFunction (Expression Float (Qty units)) (Float -> Qty units) where
  valueFunction (Curve1d expression) = do
    let f = curve1d_value_function (opensolid_curve1d_value_function (Expression1d.ptr expression))
    \(Qty.Qty x) -> Qty.Qty (f x)

instance BoundsFunction (Expression Float (Qty units)) (Range Unitless -> Range units) where
  boundsFunction (Curve1d expression) = do
    let f = curve1d_bounds_function (opensolid_curve1d_bounds_function (Expression1d.ptr expression))
    \(Range (Qty.Qty xLow) (Qty.Qty xHigh)) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f xLow xHigh outputs
      yLow <- Foreign.peekElemOff outputs 0
      yHigh <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty.Qty yLow) (Qty.Qty yHigh))

instance ValueFunction (Expression Uv.Point (Qty units)) (Uv.Point -> Qty units) where
  valueFunction (Surface1d expression) = do
    let f = surface1d_value_function (opensolid_surface1d_value_function (Expression1d.ptr expression))
    \(Point2d.Point2d (Qty.Qty x) (Qty.Qty y)) -> Qty.Qty (f x y)

instance BoundsFunction (Expression Uv.Point (Qty units)) (Uv.Bounds -> Range units) where
  boundsFunction (Surface1d expression) = do
    let expressionPtr = Expression1d.ptr expression
    let nativeFunction = opensolid_surface1d_bounds_function expressionPtr
    let wrappedFunction = surface1d_bounds_function nativeFunction
    let returnedFunction uvBounds = unsafeDupablePerformIO IO.do
          let Bounds2d uRange vRange = uvBounds
          let Range (Qty.Qty uLow) (Qty.Qty uHigh) = uRange
          let Range (Qty.Qty vLow) (Qty.Qty vHigh) = vRange
          outputs <- Alloc.mallocBytes 16
          wrappedFunction uLow uHigh vLow vHigh outputs
          low <- Foreign.peekElemOff outputs 0
          high <- Foreign.peekElemOff outputs 1
          Alloc.free outputs
          IO.succeed (Range (Qty.Qty low) (Qty.Qty high))
    returnedFunction

instance
  ValueFunction
    (Expression Float (Point2d (space @ units)))
    (Float -> Point2d (space @ units))
  where
  valueFunction (Curve2d x y) = do
    let f = curve2d_value_function (opensolid_curve2d_value_function (Expression1d.ptr x) (Expression1d.ptr y))
    \(Qty.Qty tValue) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty.Qty px) (Qty.Qty py))

instance
  BoundsFunction
    (Expression Float (Point2d (space @ units)))
    (Range Unitless -> Bounds2d (space @ units))
  where
  boundsFunction (Curve2d x y) = do
    let f = curve2d_bounds_function (opensolid_curve2d_bounds_function (Expression1d.ptr x) (Expression1d.ptr y))
    \tRange -> unsafeDupablePerformIO IO.do
      let Range (Qty.Qty tLow) (Qty.Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 32
      f tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty.Qty xLow) (Qty.Qty xHigh)
      let yRange = Range (Qty.Qty yLow) (Qty.Qty yHigh)
      Alloc.free outputs
      IO.succeed (Bounds2d xRange yRange)

instance
  ValueFunction
    (Expression Uv.Point (Point2d (space @ units)))
    (Uv.Point -> Point2d (space @ units))
  where
  valueFunction (Surface2d x y) = do
    let f = surface2d_value_function (opensolid_surface2d_value_function (Expression1d.ptr x) (Expression1d.ptr y))
    \(Point2d.Point2d (Qty.Qty uValue) (Qty.Qty vValue)) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty.Qty px) (Qty.Qty py))

instance
  BoundsFunction
    (Expression Uv.Point (Point2d (space @ units)))
    (Uv.Bounds -> Bounds2d (space @ units))
  where
  boundsFunction (Surface2d x y) = do
    let f = surface2d_bounds_function (opensolid_surface2d_bounds_function (Expression1d.ptr x) (Expression1d.ptr y))
    \uvBounds -> unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty.Qty uLow) (Qty.Qty uHigh) = uRange
      let Range (Qty.Qty vLow) (Qty.Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 32
      f uLow uHigh vLow vHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty.Qty xLow) (Qty.Qty xHigh)
      let yRange = Range (Qty.Qty yLow) (Qty.Qty yHigh)
      Alloc.free outputs
      IO.succeed (Bounds2d xRange yRange)
