module Function
  ( Function
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
import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import Function.Expression (Expression)
import Function.Expression qualified as Expression
import IO qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Point3d (Point3d (Point3d))
import Point3d qualified
import Qty (Qty (Qty))
import Qty qualified
import Range (Range (Range))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import Prelude (Double)

type role Function nominal nominal

data Function input output where
  Curve1d ::
    { c1x :: Expression Float
    , c1v :: ~Curve1dValueFunction
    , c1b :: ~Curve1dBoundsFunction
    , c1d :: ~(Function Float (Qty units))
    } ->
    Function Float (Qty units)
  Surface1d ::
    { s1x :: Expression Uv.Point
    , s1v :: ~Surface1dValueFunction
    , s1b :: ~Surface1dBoundsFunction
    , s1du :: ~(Function Uv.Point (Qty units))
    , s1dv :: ~(Function Uv.Point (Qty units))
    } ->
    Function Uv.Point (Qty units)
  Curve2d ::
    Expression Float ->
    Expression Float ->
    Function Float (Point2d (space @ units))
  Surface2d ::
    Expression Uv.Point ->
    Expression Uv.Point ->
    Function Uv.Point (Point2d (space @ units))
  VectorCurve2d ::
    Expression Float ->
    Expression Float ->
    Function Float (Vector2d (space @ units))
  VectorSurface2d ::
    Expression Uv.Point ->
    Expression Uv.Point ->
    Function Uv.Point (Vector2d (space @ units))
  Curve3d ::
    Expression Float ->
    Expression Float ->
    Expression Float ->
    Function Float (Point3d (space @ units))
  Surface3d ::
    Expression Uv.Point ->
    Expression Uv.Point ->
    Expression Uv.Point ->
    Function Uv.Point (Point3d (space @ units))
  VectorCurve3d ::
    Expression Float ->
    Expression Float ->
    Expression Float ->
    Function Float (Vector3d (space @ units))
  VectorSurface3d ::
    Expression Uv.Point ->
    Expression Uv.Point ->
    Expression Uv.Point ->
    Function Uv.Point (Vector3d (space @ units))

curve1d :: Expression Float -> Function Float (Qty units)
curve1d expression = do
  let px = Expression.ptr expression
  Curve1d
    { c1x = expression
    , c1v = curve1d_value_function (opensolid_curve1d_value_function px)
    , c1b = curve1d_bounds_function (opensolid_curve1d_bounds_function px)
    , c1d = curve1d (Expression.curveDerivative expression)
    }

surface1d :: Expression Uv.Point -> Function Uv.Point (Qty units)
surface1d expression = do
  let px = Expression.ptr expression
  let du = surface1d (Expression.surfaceDerivative U expression)
  let dv = surface1dv (Expression.surfaceDerivative V expression) du
  Surface1d
    { s1x = expression
    , s1v = surface1d_value_function (opensolid_surface1d_value_function px)
    , s1b = surface1d_bounds_function (opensolid_surface1d_bounds_function px)
    , s1du = du
    , s1dv = dv
    }

surface1dv :: Expression Uv.Point -> Function Uv.Point (Qty units) -> Function Uv.Point (Qty units)
surface1dv expression sibling = do
  let px = Expression.ptr expression
  let du = s1dv sibling
  let dv = surface1dv (Expression.surfaceDerivative V expression) du
  Surface1d
    { s1x = expression
    , s1v = surface1d_value_function (opensolid_surface1d_value_function px)
    , s1b = surface1d_bounds_function (opensolid_surface1d_bounds_function px)
    , s1du = du
    , s1dv = dv
    }

-------------
--- UNITS ---
-------------

instance HasUnits (Function input (Qty units)) where
  type UnitsOf (Function input (Qty units)) = units

instance HasUnits (Function input (Vector2d (space @ units))) where
  type UnitsOf (Function input (Vector2d (space @ units))) = units

instance HasUnits (Function input (Vector3d (space @ units))) where
  type UnitsOf (Function input (Vector3d (space @ units))) = units

instance HasUnits (Function input (Point2d (space @ units))) where
  type UnitsOf (Function input (Point2d (space @ units))) = units

instance HasUnits (Function input (Point3d (space @ units))) where
  type UnitsOf (Function input (Point3d (space @ units))) = units

instance
  input1 ~ input2 =>
  Units.Coercion (Function input1 (Qty units1)) (Function input2 (Qty units2))
  where
  coerce Curve1d{c1x, c1v, c1b, c1d} =
    Curve1d{c1x, c1v, c1b, c1d = Units.coerce c1d}
  coerce Surface1d{s1x, s1v, s1b, s1du, s1dv} =
    Surface1d{s1x, s1v, s1b, s1du = Units.coerce s1du, s1dv = Units.coerce s1dv}

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Vector2d (space1 @ units1)))
    (Function input2 (Vector2d (space2 @ units2)))
  where
  coerce (VectorCurve2d x y) = VectorCurve2d x y
  coerce (VectorSurface2d x y) = VectorSurface2d x y

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Vector3d (space1 @ units1)))
    (Function input2 (Vector3d (space2 @ units2)))
  where
  coerce (VectorCurve3d x y z) = VectorCurve3d x y z
  coerce (VectorSurface3d x y z) = VectorSurface3d x y z

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input1 (Point2d (space1 @ units1)))
    (Function input2 (Point2d (space2 @ units2)))
  where
  coerce (Curve2d x y) = Curve2d x y
  coerce (Surface2d x y) = Surface2d x y

instance
  (input1 ~ input2, space1 ~ space2) =>
  Units.Coercion
    (Function input (Point3d (space1 @ units1)))
    (Function input (Point3d (space2 @ units2)))
  where
  coerce (Curve3d x y z) = Curve3d x y z
  coerce (Surface3d x y z) = Surface3d x y z

----------------
--- NEGATION ---
----------------

instance Negation (Function input (Qty units)) where
  negate Curve1d{c1x} = curve1d (Expression.negated c1x)
  negate Surface1d{s1x} = surface1d (Expression.negated s1x)

instance Negation (Function input (Vector2d (space @ units))) where
  negate (VectorCurve2d x y) = VectorCurve2d (Expression.negated x) (Expression.negated y)
  negate (VectorSurface2d x y) = VectorSurface2d (Expression.negated x) (Expression.negated y)

instance Negation (Function input (Vector3d (space @ units))) where
  negate (VectorCurve3d x y z) =
    VectorCurve3d (Expression.negated x) (Expression.negated y) (Expression.negated z)
  negate (VectorSurface3d x y z) =
    VectorSurface3d (Expression.negated x) (Expression.negated y) (Expression.negated z)

instance Multiplication' Sign (Function input (Qty units)) where
  type Sign .*. Function input (Qty units) = Function input (Qty (Unitless :*: units))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Function input (Qty units)) Sign where
  type Function input (Qty units) .*. Sign = Function input (Qty (units :*: Unitless))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' Sign (Function input (Vector2d (space @ units))) where
  type
    Sign .*. Function input (Vector2d (space @ units)) =
      Function input (Vector2d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Function input (Vector2d (space @ units))) Sign where
  type
    Function input (Vector2d (space @ units)) .*. Sign =
      Function input (Vector2d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication' Sign (Function input (Vector3d (space @ units))) where
  type
    Sign .*. Function input (Vector3d (space @ units)) =
      Function input (Vector3d (space @ (Unitless :*: units)))
  Positive .*. expression = Units.coerce expression
  Negative .*. expression = Units.coerce -expression

instance Multiplication' (Function input (Vector3d (space @ units))) Sign where
  type
    Function input (Vector3d (space @ units)) .*. Sign =
      Function input (Vector3d (space @ (units :*: Unitless)))
  expression .*. Positive = Units.coerce expression
  expression .*. Negative = Units.coerce -expression

instance Multiplication Sign (Function input (Qty units)) (Function input (Qty units))

instance Multiplication (Function input (Qty units)) Sign (Function input (Qty units))

instance
  Multiplication
    Sign
    (Function input (Vector2d (space @ units)))
    (Function input (Vector2d (space @ units)))

instance
  Multiplication
    (Function input (Vector2d (space @ units)))
    Sign
    (Function input (Vector2d (space @ units)))

instance
  Multiplication
    Sign
    (Function input (Vector3d (space @ units)))
    (Function input (Vector3d (space @ units)))

instance
  Multiplication
    (Function input (Vector3d (space @ units)))
    Sign
    (Function input (Vector3d (space @ units)))

----------------
--- ADDITION ---
----------------

instance
  units1 ~ units2 =>
  Addition
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units1))
  where
  Curve1d{c1x = lhs} + Curve1d{c1x = rhs} = curve1d (Expression.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units1))
  where
  Surface1d{s1x = lhs} + Surface1d{s1x = rhs} = surface1d (Expression.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units1))
  where
  expression + value = expression + constant @Float value

instance
  units1 ~ units2 =>
  Addition (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units1))
  where
  expression + value = expression + constant @Uv.Point value

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units1))
  where
  value + expression = constant @Float value + expression

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units1))
  where
  value + expression = constant @Uv.Point value + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d x1 y1 + VectorCurve2d x2 y2 =
    VectorCurve2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  VectorSurface2d x1 y1 + VectorSurface2d x2 y2 =
    VectorSurface2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  vector + expression = constant @Float vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  vector + expression = constant @Uv.Point vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d x1 y1 z1 + VectorCurve3d x2 y2 z2 =
    VectorCurve3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  VectorSurface3d x1 y1 z1 + VectorSurface3d x2 y2 z2 =
    VectorSurface3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  vector + expression = constant @Float vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  vector + expression = constant @Uv.Point vector + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  Curve2d x1 y1 + VectorCurve2d x2 y2 =
    Curve2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  Surface2d x1 y1 + VectorSurface2d x2 y2 =
    Surface2d (Expression.sum x1 x2) (Expression.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Point2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  point + expression = constant @Float point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  point + expression = constant @Uv.Point point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  Curve3d x1 y1 z1 + VectorCurve3d x2 y2 z2 =
    Curve3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  Surface3d x1 y1 z1 + VectorSurface3d x2 y2 z2 =
    Surface3d (Expression.sum x1 x2) (Expression.sum y1 y2) (Expression.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Float (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Point3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  expression + vector = expression + constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  point + expression = constant @Float point + expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  point + expression = constant @Uv.Point point + expression

-------------------
--- SUBTRACTION ---
-------------------

instance
  units1 ~ units2 =>
  Subtraction
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units1))
  where
  Curve1d{c1x = lhs} - Curve1d{c1x = rhs} = curve1d (Expression.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units1))
  where
  Surface1d{s1x = lhs} - Surface1d{s1x = rhs} = surface1d (Expression.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units1))
  where
  expression - value = expression - constant @Float value

instance
  units1 ~ units2 =>
  Subtraction (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units1))
  where
  expression - value = expression - constant @Uv.Point value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units1))
  where
  value - expression = constant @Float value - expression

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units1))
  where
  value - expression = constant @Uv.Point value - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  VectorCurve2d x1 y1 - VectorCurve2d x2 y2 =
    VectorCurve2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  VectorSurface2d x1 y1 - VectorSurface2d x2 y2 =
    VectorSurface2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  vector - expression = constant @Float vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  vector - expression = constant @Uv.Point vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  VectorCurve3d x1 y1 z1 - VectorCurve3d x2 y2 z2 =
    VectorCurve3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  VectorSurface3d x1 y1 z1 - VectorSurface3d x2 y2 z2 =
    VectorSurface3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  vector - expression = constant @Float vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  vector - expression = constant @Uv.Point vector - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  Curve2d x1 y1 - VectorCurve2d x2 y2 =
    Curve2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  Surface2d x1 y1 - VectorSurface2d x2 y2 =
    Surface2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Point2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Point2d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Point2d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Function Float (Point2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  Curve2d x1 y1 - Curve2d x2 y2 =
    VectorCurve2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Function Uv.Point (Point2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  Surface2d x1 y1 - Surface2d x2 y2 =
    VectorSurface2d (Expression.difference x1 x2) (Expression.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Function Float (Vector2d (space1 @ units1)))
  where
  expression - point = expression - constant @Float point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  expression - point = expression - constant @Uv.Point point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Float (Point2d (space2 @ units2)))
    (Function Float (Vector2d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function Uv.Point (Point2d (space2 @ units2)))
    (Function Uv.Point (Vector2d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  Curve3d x1 y1 z1 - VectorCurve3d x2 y2 z2 =
    Curve3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  Surface3d x1 y1 z1 - VectorSurface3d x2 y2 z2 =
    Surface3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Float (Point3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Float vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  expression - vector = expression - constant @Uv.Point vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Float (Vector3d (space2 @ units2)))
    (Function Float (Point3d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Uv.Point (Vector3d (space2 @ units2)))
    (Function Uv.Point (Point3d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Function Float (Point3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  Curve3d x1 y1 z1 - Curve3d x2 y2 z2 =
    VectorCurve3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Function Uv.Point (Point3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  Surface3d x1 y1 z1 - Surface3d x2 y2 z2 =
    VectorSurface3d
      (Expression.difference x1 x2)
      (Expression.difference y1 y2)
      (Expression.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Float (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Function Float (Vector3d (space1 @ units1)))
  where
  expression - point = expression - constant @Float point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function Uv.Point (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  expression - point = expression - constant @Uv.Point point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Float (Point3d (space2 @ units2)))
    (Function Float (Vector3d (space1 @ units1)))
  where
  point - expression = constant @Float point - expression

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Function Uv.Point (Point3d (space2 @ units2)))
    (Function Uv.Point (Vector3d (space1 @ units1)))
  where
  point - expression = constant @Uv.Point point - expression

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
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units3))

--- Qty-Vector2d ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Qty units1))
    (Function Float (Vector2d (space @ units2)))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Vector2d (space @ units2)))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Function Float (Vector2d (space @ units2)))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Function Uv.Point (Vector2d (space @ units2)))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Qty units1))
    (Vector2d (space @ units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Qty units1))
    (Vector2d (space @ units2))
    (Function Uv.Point (Vector2d (space @ units3)))

-- Qty-Direction2d --
---------------------

instance
  Multiplication
    (Function Float (Qty units))
    (Direction2d space)
    (Function Float (Vector2d (space @ units)))

instance
  Multiplication
    (Function Uv.Point (Qty units))
    (Direction2d space)
    (Function Uv.Point (Vector2d (space @ units)))

--- Vector2d-Qty ---
--------------------

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Vector2d (space @ units1)))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Vector2d (space @ units1)))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Float (Vector2d (space @ units1)))
    (Qty units2)
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function Uv.Point (Vector2d (space @ units1)))
    (Qty units2)
    (Function Uv.Point (Vector2d (space @ units3)))

--- Direction2d-Qty ---
-----------------------

instance
  Multiplication
    (Direction2d space)
    (Function Float (Qty units))
    (Function Float (Vector2d (space @ units)))

instance
  Multiplication
    (Direction2d space)
    (Function Uv.Point (Qty units))
    (Function Uv.Point (Vector2d (space @ units)))

---------------------------------
--- Multiplication' instances ---
---------------------------------

--- Qty-Qty ---
---------------

instance Multiplication' (Function Float (Qty units1)) (Function Float (Qty units2)) where
  type
    Function Float (Qty units1) .*. Function Float (Qty units2) =
      Function Float (Qty (units1 :*: units2))
  Curve1d{c1x = lhs} .*. Curve1d{c1x = rhs} = curve1d (Expression.product lhs rhs)

instance Multiplication' (Function Uv.Point (Qty units1)) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Qty units1) .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  Surface1d{s1x = lhs} .*. Surface1d{s1x = rhs} = surface1d (Expression.product lhs rhs)

instance Multiplication' (Qty units1) (Function Float (Qty units2)) where
  type Qty units1 .*. Function Float (Qty units2) = Function Float (Qty (units1 :*: units2))
  value .*. expression = constant @Float value .*. expression

instance Multiplication' (Qty units1) (Function Uv.Point (Qty units2)) where
  type
    Qty units1 .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  value .*. expression = constant @Uv.Point value .*. expression

instance Multiplication' (Function Float (Qty units1)) (Qty units2) where
  type Function Float (Qty units1) .*. Qty units2 = Function Float (Qty (units1 :*: units2))
  expression .*. value = expression .*. constant @Float value

instance Multiplication' (Function Uv.Point (Qty units1)) (Qty units2) where
  type
    Function Uv.Point (Qty units1) .*. Qty units2 =
      Function Uv.Point (Qty (units1 :*: units2))
  expression .*. value = expression .*. constant @Uv.Point value

--- Qty-Vector2d ---
--------------------

instance Multiplication' (Function Float (Qty units1)) (Function Float (Vector2d (space @ units2))) where
  type
    Function Float (Qty units1) .*. Function Float (Vector2d (space @ units2)) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  Curve1d{c1x = scale} .*. VectorCurve2d x y =
    VectorCurve2d (Expression.product scale x) (Expression.product scale y)

instance Multiplication' (Function Uv.Point (Qty units1)) (Function Uv.Point (Vector2d (space @ units2))) where
  type
    Function Uv.Point (Qty units1) .*. Function Uv.Point (Vector2d (space @ units2)) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  Surface1d{s1x = scale} .*. VectorSurface2d x y =
    VectorSurface2d (Expression.product scale x) (Expression.product scale y)

instance Multiplication' (Qty units1) (Function Float (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Function Float (Vector2d (space @ units2)) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  value .*. expression = constant @Float value .*. expression

instance Multiplication' (Qty units1) (Function Uv.Point (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Function Uv.Point (Vector2d (space @ units2)) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  value .*. expression = constant @Uv.Point value .*. expression

instance Multiplication' (Function Float (Qty units1)) (Vector2d (space @ units2)) where
  type
    Function Float (Qty units1) .*. Vector2d (space @ units2) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. constant @Float vector

instance Multiplication' (Function Uv.Point (Qty units1)) (Vector2d (space @ units2)) where
  type
    Function Uv.Point (Qty units1) .*. Vector2d (space @ units2) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. constant @Uv.Point vector

--- Qty-Direction2d ---
-----------------------

instance Multiplication' (Function Float (Qty units)) (Direction2d space) where
  type
    Function Float (Qty units) .*. Direction2d space =
      Function Float (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

instance Multiplication' (Function Uv.Point (Qty units)) (Direction2d space) where
  type
    Function Uv.Point (Qty units) .*. Direction2d space =
      Function Uv.Point (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

--- Vector2d-Qty ---
--------------------

instance Multiplication' (Function Float (Vector2d (space @ units1))) (Function Float (Qty units2)) where
  type
    Function Float (Vector2d (space @ units1)) .*. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  VectorCurve2d x y .*. Curve1d{c1x = scale} =
    VectorCurve2d (Expression.product x scale) (Expression.product y scale)

instance Multiplication' (Function Uv.Point (Vector2d (space @ units1))) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Vector2d (space @ units1)) .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  VectorSurface2d x y .*. Surface1d{s1x = scale} =
    VectorSurface2d (Expression.product x scale) (Expression.product y scale)

instance Multiplication' (Vector2d (space @ units1)) (Function Float (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = constant @Float vector .*. expression

instance Multiplication' (Vector2d (space @ units1)) (Function Uv.Point (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = constant @Uv.Point vector .*. expression

instance Multiplication' (Function Float (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Float (Vector2d (space @ units1)) .*. Qty units2 =
      Function Float (Vector2d (space @ (units1 :*: units2)))
  expression .*. value = expression .*. constant @Float value

instance Multiplication' (Function Uv.Point (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Uv.Point (Vector2d (space @ units1)) .*. Qty units2 =
      Function Uv.Point (Vector2d (space @ (units1 :*: units2)))
  expression .*. value = expression .*. constant @Uv.Point value

--- Direction2d-Qty ---
-----------------------

instance Multiplication' (Direction2d space) (Function Float (Qty units)) where
  type
    Direction2d space .*. Function Float (Qty units) =
      Function Float (Vector2d (space @ (Unitless :*: units)))
  direction .*. scale = Vector2d.unit direction .*. scale

instance Multiplication' (Direction2d space) (Function Uv.Point (Qty units)) where
  type
    Direction2d space .*. Function Uv.Point (Qty units) =
      Function Uv.Point (Vector2d (space @ (Unitless :*: units)))
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
    (Function Float (Qty units1))
    (Function Float (Qty units2))
    (Function Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Uv.Point (Qty units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function Float (Qty units1)) (Qty units2) (Function Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function Uv.Point (Qty units1)) (Qty units2) (Function Uv.Point (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function Float (Qty units2)) (Function Float (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function Uv.Point (Qty units2)) (Function Uv.Point (Qty units3))

--- Vector2d-Qty ---
--------------------

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Float (Vector2d (space @ units1)))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Uv.Point (Vector2d (space @ units1)))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Function Float (Qty units2))
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Function Uv.Point (Qty units2))
    (Function Uv.Point (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Float (Vector2d (space @ units1)))
    (Qty units2)
    (Function Float (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function Uv.Point (Vector2d (space @ units1)))
    (Qty units2)
    (Function Uv.Point (Vector2d (space @ units3)))

---------------------------
--- Division' instances ---
---------------------------

--- Qty-Qty ---
---------------

instance Division' (Function Float (Qty units1)) (Function Float (Qty units2)) where
  type
    Function Float (Qty units1) ./. Function Float (Qty units2) =
      Function Float (Qty (units1 :/: units2))
  Curve1d{c1x = lhs} ./. Curve1d{c1x = rhs} = curve1d (Expression.quotient lhs rhs)

instance Division' (Function Uv.Point (Qty units1)) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Qty units1) ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :/: units2))
  Surface1d{s1x = lhs} ./. Surface1d{s1x = rhs} = surface1d (Expression.quotient lhs rhs)

instance Division' (Function Float (Qty units1)) (Qty units2) where
  type Function Float (Qty units1) ./. Qty units2 = Function Float (Qty (units1 :/: units2))
  expression ./. value = expression ./. constant @Float value

instance Division' (Function Uv.Point (Qty units1)) (Qty units2) where
  type
    Function Uv.Point (Qty units1) ./. Qty units2 =
      Function Uv.Point (Qty (units1 :/: units2))
  expression ./. value = expression ./. constant @Uv.Point value

instance Division' (Qty units1) (Function Float (Qty units2)) where
  type Qty units1 ./. Function Float (Qty units2) = Function Float (Qty (units1 :/: units2))
  value ./. expression = constant @Float value ./. expression

instance Division' (Qty units1) (Function Uv.Point (Qty units2)) where
  type
    Qty units1 ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Qty (units1 :/: units2))
  value ./. expression = constant @Uv.Point value ./. expression

--- Vector2d-Qty ---
--------------------

instance Division' (Function Float (Vector2d (space @ units1))) (Function Float (Qty units2)) where
  type
    Function Float (Vector2d (space @ units1)) ./. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :/: units2)))
  VectorCurve2d x y ./. Curve1d{c1x = scale} =
    VectorCurve2d (Expression.quotient x scale) (Expression.quotient y scale)

instance Division' (Function Uv.Point (Vector2d (space @ units1))) (Function Uv.Point (Qty units2)) where
  type
    Function Uv.Point (Vector2d (space @ units1)) ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :/: units2)))
  VectorSurface2d x y ./. Surface1d{s1x = scale} =
    VectorSurface2d (Expression.quotient x scale) (Expression.quotient y scale)

instance Division' (Vector2d (space @ units1)) (Function Float (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Function Float (Qty units2) =
      Function Float (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = constant @Float vector ./. expression

instance Division' (Vector2d (space @ units1)) (Function Uv.Point (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Function Uv.Point (Qty units2) =
      Function Uv.Point (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = constant @Uv.Point vector ./. expression

instance Division' (Function Float (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Float (Vector2d (space @ units1)) ./. Qty units2 =
      Function Float (Vector2d (space @ (units1 :/: units2)))
  expression ./. value = expression ./. constant @Float value

instance Division' (Function Uv.Point (Vector2d (space @ units1))) (Qty units2) where
  type
    Function Uv.Point (Vector2d (space @ units1)) ./. Qty units2 =
      Function Uv.Point (Vector2d (space @ (units1 :/: units2)))
  expression ./. value = expression ./. constant @Uv.Point value

-------------------
--- DOT PRODUCT ---
-------------------

--- DotMultiplication instances ---
-----------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Qty units3))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
    (Function Uv.Point (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Uv.Point (Qty units))

--- DotMultiplication' instances ---
------------------------------------

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Function Float (Vector2d (space1 @ units1))
      .<>. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  VectorCurve2d x1 y1 .<>. VectorCurve2d x2 y2 =
    curve1d (Expression.sum (Expression.product x1 x2) (Expression.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1))
      .<>. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  VectorSurface2d x1 y1 .<>. VectorSurface2d x2 y2 =
    surface1d (Expression.sum (Expression.product x1 x2) (Expression.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Float (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Function Float (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. constant @Float vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. constant @Uv.Point vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  vector .<>. expression = constant @Float vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  vector .<>. expression = constant @Uv.Point vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Float (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Function Float (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Uv.Point (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Function Uv.Point (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Function Float (Vector2d (space2 @ units)) =
      Function Float (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Function Uv.Point (Vector2d (space2 @ units)) =
      Function Uv.Point (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

---------------------
--- CROSS PRODUCT ---
---------------------

--- CrossMultiplication instances ---
-------------------------------------

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
    (Function Uv.Point (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Float (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Function Uv.Point (Qty units3))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
    (Function Uv.Point (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Float (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Function Uv.Point (Qty units))

--- CrossMultiplication' instances ---
--------------------------------------

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Function Float (Vector2d (space1 @ units1))
      .><. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  VectorCurve2d x1 y1 .><. VectorCurve2d x2 y2 =
    curve1d (Expression.difference (Expression.product x1 y2) (Expression.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1))
      .><. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  VectorSurface2d x1 y1 .><. VectorSurface2d x2 y2 =
    surface1d (Expression.difference (Expression.product x1 y2) (Expression.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Float (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Float (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Function Float (Qty (units1 :*: units2))
  expression .><. vector = expression .><. constant @Float vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Function Uv.Point (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Function Uv.Point (Qty (units1 :*: units2))
  expression .><. vector = expression .><. constant @Uv.Point vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Function Float (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Function Float (Vector2d (space2 @ units2)) =
      Function Float (Qty (units1 :*: units2))
  vector .><. expression = constant @Float vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Function Uv.Point (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Function Uv.Point (Vector2d (space2 @ units2)) =
      Function Uv.Point (Qty (units1 :*: units2))
  vector .><. expression = constant @Uv.Point vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Float (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Float (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Function Float (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function Uv.Point (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Function Uv.Point (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Function Uv.Point (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Function Float (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Function Float (Vector2d (space2 @ units)) =
      Function Float (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Function Uv.Point (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Function Uv.Point (Vector2d (space2 @ units)) =
      Function Uv.Point (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Function Float Float)
    (Function Float output)
    (Function Float output)
  where
  curve . Curve1d{c1x = input} =
    case curve of
      Curve1d{c1x} -> curve1d (c1x . input)
      Curve2d x y -> Curve2d (x . input) (y . input)
      VectorCurve2d x y -> VectorCurve2d (x . input) (y . input)
      Curve3d x y z -> Curve3d (x . input) (y . input) (z . input)
      VectorCurve3d x y z -> VectorCurve3d (x . input) (y . input) (z . input)

instance
  Composition
    (Function Uv.Point Float)
    (Function Float output)
    (Function Uv.Point output)
  where
  curve . Surface1d{s1x = input} =
    case curve of
      Curve1d{c1x} -> surface1d (c1x . input)
      Curve2d x y -> Surface2d (x . input) (y . input)
      VectorCurve2d x y -> VectorSurface2d (x . input) (y . input)
      Curve3d x y z -> Surface3d (x . input) (y . input) (z . input)
      VectorCurve3d x y z -> VectorSurface3d (x . input) (y . input) (z . input)

instance
  Composition
    (Function Float Uv.Point)
    (Function Uv.Point output)
    (Function Float output)
  where
  surface . Curve2d uInput vInput = do
    let inputs = (uInput, vInput)
    case surface of
      Surface1d{s1x} -> curve1d (s1x . inputs)
      Surface2d x y -> Curve2d (x . inputs) (y . inputs)
      VectorSurface2d x y -> VectorCurve2d (x . inputs) (y . inputs)
      Surface3d x y z -> Curve3d (x . inputs) (y . inputs) (z . inputs)
      VectorSurface3d x y z -> VectorCurve3d (x . inputs) (y . inputs) (z . inputs)

instance
  Composition
    (Function Uv.Point Uv.Point)
    (Function Uv.Point output)
    (Function Uv.Point output)
  where
  surface . Surface2d uInput vInput = do
    let inputs = (uInput, vInput)
    case surface of
      Surface1d{s1x} -> surface1d (s1x . inputs)
      Surface2d x y -> Surface2d (x . inputs) (y . inputs)
      VectorSurface2d x y -> VectorSurface2d (x . inputs) (y . inputs)
      Surface3d x y z -> Surface3d (x . inputs) (y . inputs) (z . inputs)
      VectorSurface3d x y z -> VectorSurface3d (x . inputs) (y . inputs) (z . inputs)

-----------------
--- FUNCTIONS ---
-----------------

class Zero input output where
  zero :: Function input output

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
  origin :: Function input output

instance Origin Float (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Uv.Point (Point2d (space @ units)) where
  origin = constant Point2d.origin

instance Origin Float (Point3d (space @ units)) where
  origin = constant Point3d.origin

instance Origin Uv.Point (Point3d (space @ units)) where
  origin = constant Point3d.origin

class Constant input output where
  constant :: output -> Function input output

instance Constant Float (Qty units) where
  constant value = curve1d (Expression.constant value)

instance Constant Uv.Point (Qty units) where
  constant value = surface1d (Expression.constant value)

instance Constant Float (Vector2d (space @ units)) where
  constant (Vector2d x y) =
    VectorCurve2d (Expression.constant x) (Expression.constant y)

instance Constant Uv.Point (Vector2d (space @ units)) where
  constant (Vector2d x y) =
    VectorSurface2d (Expression.constant x) (Expression.constant y)

instance Constant Float (Vector3d (space @ units)) where
  constant (Vector3d x y z) =
    VectorCurve3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

instance Constant Uv.Point (Vector3d (space @ units)) where
  constant (Vector3d x y z) =
    VectorSurface3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

instance Constant Float (Point2d (space @ units)) where
  constant (Point2d x y) =
    Curve2d (Expression.constant x) (Expression.constant y)

instance Constant Uv.Point (Point2d (space @ units)) where
  constant (Point2d x y) =
    Surface2d (Expression.constant x) (Expression.constant y)

instance Constant Float (Point3d (space @ units)) where
  constant (Point3d x y z) =
    Curve3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

instance Constant Uv.Point (Point3d (space @ units)) where
  constant (Point3d x y z) =
    Surface3d (Expression.constant x) (Expression.constant y) (Expression.constant z)

class XY input output where
  xy ::
    Function input (Qty (UnitsOf output)) ->
    Function input (Qty (UnitsOf output)) ->
    Function input output

instance XY Float (Vector2d (space @ units)) where
  xy Curve1d{c1x = x} Curve1d{c1x = y} = VectorCurve2d x y

instance XY Uv.Point (Vector2d (space @ units)) where
  xy Surface1d{s1x = x} Surface1d{s1x = y} = VectorSurface2d x y

instance XY Float (Point2d (space @ units)) where
  xy Curve1d{c1x = x} Curve1d{c1x = y} = Curve2d x y

instance XY Uv.Point (Point2d (space @ units)) where
  xy Surface1d{s1x = x} Surface1d{s1x = y} = Surface2d x y

class XYZ input output where
  xyz ::
    Function input (Qty (UnitsOf output)) ->
    Function input (Qty (UnitsOf output)) ->
    Function input (Qty (UnitsOf output)) ->
    Function input output

instance XYZ Float (Vector3d (space @ units)) where
  xyz Curve1d{c1x = x} Curve1d{c1x = y} Curve1d{c1x = z} = VectorCurve3d x y z

instance XYZ Uv.Point (Vector3d (space @ units)) where
  xyz Surface1d{s1x = x} Surface1d{s1x = y} Surface1d{s1x = z} = VectorSurface3d x y z

instance XYZ Float (Point3d (space @ units)) where
  xyz Curve1d{c1x = x} Curve1d{c1x = y} Curve1d{c1x = z} = Curve3d x y z

instance XYZ Uv.Point (Point3d (space @ units)) where
  xyz Surface1d{s1x = x} Surface1d{s1x = y} Surface1d{s1x = z} = Surface3d x y z

class XComponent input output where
  xComponent :: Function input output -> Function input (Qty (UnitsOf output))

class YComponent input output where
  yComponent :: Function input output -> Function input (Qty (UnitsOf output))

class ZComponent input output where
  zComponent :: Function input output -> Function input (Qty (UnitsOf output))

instance XComponent input (Vector2d (space @ units)) where
  xComponent (VectorCurve2d x _) = curve1d x
  xComponent (VectorSurface2d x _) = surface1d x

instance XComponent input (Vector3d (space @ units)) where
  xComponent (VectorCurve3d x _ _) = curve1d x
  xComponent (VectorSurface3d x _ _) = surface1d x

instance YComponent input (Vector2d (space @ units)) where
  yComponent (VectorCurve2d _ y) = curve1d y
  yComponent (VectorSurface2d _ y) = surface1d y

instance YComponent input (Vector3d (space @ units)) where
  yComponent (VectorCurve3d _ y _) = curve1d y
  yComponent (VectorSurface3d _ y _) = surface1d y

instance ZComponent input (Vector3d (space @ units)) where
  zComponent (VectorCurve3d _ _ z) = curve1d z
  zComponent (VectorSurface3d _ _ z) = surface1d z

class XCoordinate input output where
  xCoordinate :: Function input output -> Function input (Qty (UnitsOf output))

class YCoordinate input output where
  yCoordinate :: Function input output -> Function input (Qty (UnitsOf output))

class ZCoordinate input output where
  zCoordinate :: Function input output -> Function input (Qty (UnitsOf output))

instance XCoordinate input (Point2d (space @ units)) where
  xCoordinate (Curve2d x _) = curve1d x
  xCoordinate (Surface2d x _) = surface1d x

instance XCoordinate input (Point3d (space @ units)) where
  xCoordinate (Curve3d x _ _) = curve1d x
  xCoordinate (Surface3d x _ _) = surface1d x

instance YCoordinate input (Point2d (space @ units)) where
  yCoordinate (Curve2d _ y) = curve1d y
  yCoordinate (Surface2d _ y) = surface1d y

instance YCoordinate input (Point3d (space @ units)) where
  yCoordinate (Curve3d _ y _) = curve1d y
  yCoordinate (Surface3d _ y _) = surface1d y

instance ZCoordinate input (Point3d (space @ units)) where
  zCoordinate (Curve3d _ _ z) = curve1d z
  zCoordinate (Surface3d _ _ z) = surface1d z

parameter :: Function Float Float
parameter = curve1d Expression.parameter

u :: Function Uv.Point Float
u = surface1d Expression.u

v :: Function Uv.Point Float
v = surface1d Expression.v

squared' :: Function input (Qty units) -> Function input (Qty (units :*: units))
squared' Curve1d{c1x} = curve1d (Expression.squared c1x)
squared' Surface1d{s1x} = surface1d (Expression.squared s1x)

squared ::
  Units.Squared units1 units2 =>
  Function input (Qty units1) ->
  Function input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Function input (Qty (units :*: units)) -> Function input (Qty units)
sqrt' Curve1d{c1x} = curve1d (Expression.sqrt c1x)
sqrt' Surface1d{s1x} = surface1d (Expression.sqrt s1x)

sqrt ::
  Units.Squared units1 units2 =>
  Function input (Qty units2) ->
  Function input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Function input Angle -> Function input Float
sin Curve1d{c1x} = curve1d (Expression.sin c1x)
sin Surface1d{s1x} = surface1d (Expression.sin s1x)

cos :: Function input Angle -> Function input Float
cos Curve1d{c1x} = curve1d (Expression.cos c1x)
cos Surface1d{s1x} = surface1d (Expression.cos s1x)

-----------------------
--- DIFFERENTIATION ---
-----------------------

class CurveDerivative expression derivative | expression -> derivative where
  curveDerivative :: expression -> derivative

class SurfaceDerivative expression derivative | expression -> derivative where
  surfaceDerivative :: Uv.Parameter -> expression -> derivative

instance
  CurveDerivative
    (Function Float (Qty units))
    (Function Float (Qty units))
  where
  curveDerivative = c1d

instance
  SurfaceDerivative
    (Function Uv.Point (Qty units))
    (Function Uv.Point (Qty units))
  where
  surfaceDerivative U = s1du
  surfaceDerivative V = s1dv

instance
  CurveDerivative
    (Function Float (Vector2d (space @ units)))
    (Function Float (Vector2d (space @ units)))
  where
  curveDerivative (VectorCurve2d x y) =
    VectorCurve2d
      (Expression.curveDerivative x)
      (Expression.curveDerivative y)

instance
  CurveDerivative
    (Function Float (Point2d (space @ units)))
    (Function Float (Vector2d (space @ units)))
  where
  curveDerivative (Curve2d x y) =
    VectorCurve2d
      (Expression.curveDerivative x)
      (Expression.curveDerivative y)

instance
  SurfaceDerivative
    (Function Uv.Point (Vector2d (space @ units)))
    (Function Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (VectorSurface2d x y) =
    VectorSurface2d
      (Expression.surfaceDerivative p x)
      (Expression.surfaceDerivative p y)

instance
  SurfaceDerivative
    (Function Uv.Point (Point2d (space @ units)))
    (Function Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (Surface2d x y) =
    VectorSurface2d
      (Expression.surfaceDerivative p x)
      (Expression.surfaceDerivative p y)

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
  opensolid_curve1d_value_function :: Expression.Ptr -> FunPtr Curve1dValueFunction

foreign import ccall unsafe "opensolid_curve1d_bounds_function"
  opensolid_curve1d_bounds_function :: Expression.Ptr -> FunPtr Curve1dBoundsFunction

foreign import ccall unsafe "opensolid_surface1d_value_function"
  opensolid_surface1d_value_function :: Expression.Ptr -> FunPtr Surface1dValueFunction

foreign import ccall unsafe "opensolid_surface1d_bounds_function"
  opensolid_surface1d_bounds_function :: Expression.Ptr -> FunPtr Surface1dBoundsFunction

foreign import ccall unsafe "opensolid_curve2d_value_function"
  opensolid_curve2d_value_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Curve2dValueFunction

foreign import ccall unsafe "opensolid_curve2d_bounds_function"
  opensolid_curve2d_bounds_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Curve2dBoundsFunction

foreign import ccall unsafe "opensolid_surface2d_value_function"
  opensolid_surface2d_value_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Surface2dValueFunction

foreign import ccall unsafe "opensolid_surface2d_bounds_function"
  opensolid_surface2d_bounds_function :: Expression.Ptr -> Expression.Ptr -> FunPtr Surface2dBoundsFunction

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

instance ValueFunction (Function Float (Qty units)) (Float -> Qty units) where
  valueFunction Curve1d{c1v} (Qty x) = Qty (c1v x)

instance BoundsFunction (Function Float (Qty units)) (Range Unitless -> Range units) where
  boundsFunction Curve1d{c1b} tRange =
    unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 16
      c1b tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty xLow) (Qty xHigh))

instance ValueFunction (Function Uv.Point (Qty units)) (Uv.Point -> Qty units) where
  valueFunction Surface1d{s1v} (Point2d.Point2d (Qty x) (Qty y)) = Qty (s1v x y)

instance BoundsFunction (Function Uv.Point (Qty units)) (Uv.Bounds -> Range units) where
  boundsFunction Surface1d{s1b} uvBounds =
    unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 16
      s1b uLow uHigh vLow vHigh outputs
      low <- Foreign.peekElemOff outputs 0
      high <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty low) (Qty high))

instance
  ValueFunction
    (Function Float (Point2d (space @ units)))
    (Float -> Point2d (space @ units))
  where
  valueFunction (Curve2d x y) = do
    let f = curve2d_value_function (opensolid_curve2d_value_function (Expression.ptr x) (Expression.ptr y))
    \(Qty tValue) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

instance
  BoundsFunction
    (Function Float (Point2d (space @ units)))
    (Range Unitless -> Bounds2d (space @ units))
  where
  boundsFunction (Curve2d x y) = do
    let f = curve2d_bounds_function (opensolid_curve2d_bounds_function (Expression.ptr x) (Expression.ptr y))
    \tRange -> unsafeDupablePerformIO IO.do
      let Range (Qty tLow) (Qty tHigh) = tRange
      outputs <- Alloc.mallocBytes 32
      f tLow tHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      Alloc.free outputs
      IO.succeed (Bounds2d xRange yRange)

instance
  ValueFunction
    (Function Uv.Point (Point2d (space @ units)))
    (Uv.Point -> Point2d (space @ units))
  where
  valueFunction (Surface2d x y) = do
    let f = surface2d_value_function (opensolid_surface2d_value_function (Expression.ptr x) (Expression.ptr y))
    \(Point2d.Point2d (Qty uValue) (Qty vValue)) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

instance
  BoundsFunction
    (Function Uv.Point (Point2d (space @ units)))
    (Uv.Bounds -> Bounds2d (space @ units))
  where
  boundsFunction (Surface2d x y) = do
    let f = surface2d_bounds_function (opensolid_surface2d_bounds_function (Expression.ptr x) (Expression.ptr y))
    \uvBounds -> unsafeDupablePerformIO IO.do
      let Bounds2d uRange vRange = uvBounds
      let Range (Qty uLow) (Qty uHigh) = uRange
      let Range (Qty vLow) (Qty vHigh) = vRange
      outputs <- Alloc.mallocBytes 32
      f uLow uHigh vLow vHigh outputs
      xLow <- Foreign.peekElemOff outputs 0
      xHigh <- Foreign.peekElemOff outputs 1
      yLow <- Foreign.peekElemOff outputs 2
      yHigh <- Foreign.peekElemOff outputs 3
      let xRange = Range (Qty xLow) (Qty xHigh)
      let yRange = Range (Qty yLow) (Qty yHigh)
      Alloc.free outputs
      IO.succeed (Bounds2d xRange yRange)
