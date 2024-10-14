module Expression
  ( Expression (..)
  , zero
  , constant
  , parameter
  , u
  , v
  , sqrt
  , sqrt'
  , squared
  , squared'
  , sin
  , cos
  , interpolateFrom
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
import Point2d (Point2d)
import Point2d qualified
import Point3d (Point3d)
import Point3d qualified
import Qty qualified
import Range (Range (Range))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Units qualified
import Uv qualified
import Vector2d (Vector2d)
import Vector2d qualified
import Vector3d (Vector3d)
import Vector3d qualified
import Prelude (Double)

type role Expression nominal nominal

data Expression input output where
  Qty ::
    Expression1d input ->
    Expression input (Qty units)
  Point2d ::
    Expression1d input ->
    Expression1d input ->
    Expression input (Point2d (space @ units))
  Vector2d ::
    Expression1d input ->
    Expression1d input ->
    Expression input (Vector2d (space @ units))
  Point3d ::
    Expression1d input ->
    Expression1d input ->
    Expression1d input ->
    Expression input (Point3d (space @ units))
  Vector3d ::
    Expression1d input ->
    Expression1d input ->
    Expression1d input ->
    Expression input (Vector3d (space @ units))

vector2d :: Vector2d (space @ units) -> Expression input (Vector2d (space @ units))
vector2d (Vector2d.Vector2d x y) =
  Vector2d
    (Expression1d.constant (Units.coerce x))
    (Expression1d.constant (Units.coerce y))

point2d :: Point2d (space @ units) -> Expression input (Point2d (space @ units))
point2d (Point2d.Point2d x y) =
  Point2d
    (Expression1d.constant (Units.coerce x))
    (Expression1d.constant (Units.coerce y))

vector3d :: Vector3d (space @ units) -> Expression input (Vector3d (space @ units))
vector3d (Vector3d.Vector3d x y z) =
  Vector3d
    (Expression1d.constant (Units.coerce x))
    (Expression1d.constant (Units.coerce y))
    (Expression1d.constant (Units.coerce z))

point3d :: Point3d (space @ units) -> Expression input (Point3d (space @ units))
point3d (Point3d.Point3d x y z) =
  Point3d
    (Expression1d.constant (Units.coerce x))
    (Expression1d.constant (Units.coerce y))
    (Expression1d.constant (Units.coerce z))

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
  coerce (Qty expression) = Qty expression

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Vector2d (space @ units1)))
    (Expression input2 (Vector2d (space @ units2)))
  where
  coerce (Vector2d x y) = Vector2d x y

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Vector3d (space @ units1)))
    (Expression input2 (Vector3d (space @ units2)))
  where
  coerce (Vector3d x y z) = Vector3d x y z

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Point2d (space @ units1)))
    (Expression input2 (Point2d (space @ units2)))
  where
  coerce (Point2d x y) = Point2d x y

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Point3d (space @ units1)))
    (Expression input2 (Point3d (space @ units2)))
  where
  coerce (Point3d x y z) = Point3d x y z

----------------
--- NEGATION ---
----------------

instance Negation (Expression input (Qty units)) where
  negate (Qty x) = Qty (Expression1d.negated x)

instance Negation (Expression input (Vector2d (space @ units))) where
  negate (Vector2d x y) = Vector2d (Expression1d.negated x) (Expression1d.negated y)

instance Negation (Expression input (Vector3d (space @ units))) where
  negate (Vector3d x y z) =
    Vector3d (Expression1d.negated x) (Expression1d.negated y) (Expression1d.negated z)

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
  (input1 ~ input2, units1 ~ units2) =>
  Addition
    (Expression input1 (Qty units1))
    (Expression input2 (Qty units2))
    (Expression input1 (Qty units1))
  where
  Qty lhs + Qty rhs = Qty (Expression1d.sum lhs rhs)

instance
  units1 ~ units2 =>
  Addition (Expression input (Qty units1)) (Qty units2) (Expression input (Qty units1))
  where
  expression + value = expression + constant value

instance
  units1 ~ units2 =>
  Addition (Qty units1) (Expression input (Qty units2)) (Expression input (Qty units1))
  where
  value + expression = constant value + expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Vector2d (space1 @ units1)))
  where
  Vector2d x1 y1 + Vector2d x2 y2 =
    Vector2d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Vector2d (space1 @ units1)))
  where
  expression + vector = expression + vector2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Vector2d (space1 @ units1)))
  where
  vector + expression = vector2d vector + expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Vector3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Vector3d (space1 @ units1)))
  where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 =
    Vector3d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2) (Expression1d.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Vector3d (space1 @ units1)))
  where
  expression + vector = expression + vector3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Vector3d (space1 @ units1)))
  where
  vector + expression = vector3d vector + expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Point2d (space1 @ units1)))
  where
  Point2d x1 y1 + Vector2d x2 y2 =
    Point2d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Point2d (space1 @ units1)))
  where
  expression + vector = expression + vector2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Point2d (space1 @ units1)))
  where
  point + expression = point2d point + expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Point3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Point3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 + Vector3d x2 y2 z2 =
    Point3d (Expression1d.sum x1 x2) (Expression1d.sum y1 y2) (Expression1d.sum z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Point3d (space1 @ units1)))
  where
  expression + vector = expression + vector3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Point3d (space1 @ units1)))
  where
  point + expression = point3d point + expression

-------------------
--- SUBTRACTION ---
-------------------

instance
  (input1 ~ input2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Qty units1))
    (Expression input2 (Qty units2))
    (Expression input1 (Qty units1))
  where
  Qty lhs - Qty rhs = Qty (Expression1d.difference lhs rhs)

instance
  units1 ~ units2 =>
  Subtraction (Expression input (Qty units1)) (Qty units2) (Expression input (Qty units1))
  where
  expression - value = expression - constant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Expression input (Qty units2)) (Expression input (Qty units1))
  where
  value - expression = constant value - expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Vector2d (space1 @ units1)))
  where
  Vector2d x1 y1 - Vector2d x2 y2 =
    Vector2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Vector2d (space1 @ units1)))
  where
  expression - vector = expression - vector2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Vector2d (space1 @ units1)))
  where
  vector - expression = vector2d vector - expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Vector3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Vector3d (space1 @ units1)))
  where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 =
    Vector3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Vector3d (space1 @ units1)))
  where
  expression - vector = expression - vector3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Vector3d (space1 @ units1)))
  where
  vector - expression = vector3d vector - expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Point2d (space1 @ units1)))
  where
  Point2d x1 y1 - Vector2d x2 y2 =
    Point2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Point2d (space1 @ units1)))
  where
  expression - vector = expression - vector2d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Point2d (space1 @ units1)))
  where
  point - expression = point2d point - expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Point3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 - Vector3d x2 y2 z2 =
    Point3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Point3d (space1 @ units1)))
  where
  expression - vector = expression - vector3d vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Point3d (space1 @ units1)))
  where
  point - expression = point3d point - expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Point2d (space2 @ units2)))
    (Expression input1 (Vector2d (space1 @ units1)))
  where
  Point2d x1 y1 - Point2d x2 y2 =
    Vector2d (Expression1d.difference x1 x2) (Expression1d.difference y1 y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Expression input (Vector2d (space1 @ units1)))
  where
  expression - point = expression - point2d point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression input (Point2d (space2 @ units2)))
    (Expression input (Vector2d (space1 @ units1)))
  where
  point - expression = point2d point - expression

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point3d (space1 @ units1)))
    (Expression input2 (Point3d (space2 @ units2)))
    (Expression input1 (Vector3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 =
    Vector3d
      (Expression1d.difference x1 x2)
      (Expression1d.difference y1 y2)
      (Expression1d.difference z1 z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Expression input (Vector3d (space1 @ units1)))
  where
  expression - point = expression - point3d point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression input (Point3d (space2 @ units2)))
    (Expression input (Vector3d (space1 @ units1)))
  where
  point - expression = point3d point - expression

----------------------
--- MULTIPLICATION ---
----------------------

--------------------------------
--- Multiplication instances ---
--------------------------------

--- Qty-Qty ---
---------------

instance
  (input1 ~ input2, Units.Product units1 units2 units3) =>
  Multiplication
    (Expression input1 (Qty units1))
    (Expression input2 (Qty units2))
    (Expression input1 (Qty units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Expression input (Qty units2)) (Expression input (Qty units3))

instance
  (input1 ~ input2, Units.Product units1 units2 units3) =>
  Multiplication (Expression input (Qty units1)) (Qty units2) (Expression input (Qty units3))

--- Qty-Vector2d ---
--------------------

instance
  (input1 ~ input2, Units.Product units1 units2 units3) =>
  Multiplication
    (Expression input1 (Qty units1))
    (Expression input2 (Vector2d (space @ units2)))
    (Expression input1 (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Expression input (Vector2d (space @ units2)))
    (Expression input (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression input1 (Qty units1))
    (Vector2d (space @ units2))
    (Expression input1 (Vector2d (space @ units3)))

-- Qty-Direction2d --
---------------------

instance
  Multiplication
    (Expression input (Qty units))
    (Direction2d space)
    (Expression input (Vector2d (space @ units)))

--- Vector2d-Qty ---
--------------------

instance
  (input1 ~ input2, Units.Product units1 units2 units3) =>
  Multiplication
    (Expression input1 (Vector2d (space @ units1)))
    (Expression input2 (Qty units2))
    (Expression input1 (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (Expression input (Qty units2))
    (Expression input (Vector2d (space @ units3)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Expression input (Vector2d (space @ units1)))
    (Qty units2)
    (Expression input (Vector2d (space @ units3)))

--- Direction2d-Qty ---
-----------------------

instance
  Multiplication
    (Direction2d space)
    (Expression input (Qty units))
    (Expression input (Vector2d (space @ units)))

---------------------------------
--- Multiplication' instances ---
---------------------------------

--- Qty-Qty ---
---------------

instance
  input1 ~ input2 =>
  Multiplication' (Expression input1 (Qty units1)) (Expression input2 (Qty units2))
  where
  type
    Expression input1 (Qty units1) .*. Expression input2 (Qty units2) =
      Expression input1 (Qty (units1 :*: units2))
  Qty lhs .*. Qty rhs = Qty (Expression1d.product lhs rhs)

instance Multiplication' (Qty units1) (Expression input (Qty units2)) where
  type Qty units1 .*. Expression input (Qty units2) = Expression input (Qty (units1 :*: units2))
  value .*. expression = constant value .*. expression

instance Multiplication' (Expression input (Qty units1)) (Qty units2) where
  type Expression input (Qty units1) .*. Qty units2 = Expression input (Qty (units1 :*: units2))
  expression .*. value = expression .*. constant value

--- Qty-Vector2d ---
--------------------

instance
  input1 ~ input2 =>
  Multiplication' (Expression input1 (Qty units1)) (Expression input2 (Vector2d (space @ units2)))
  where
  type
    Expression input1 (Qty units1) .*. Expression input2 (Vector2d (space @ units2)) =
      Expression input1 (Vector2d (space @ (units1 :*: units2)))
  Qty scale .*. Vector2d x y =
    Vector2d (Expression1d.product scale x) (Expression1d.product scale y)

instance Multiplication' (Qty units1) (Expression input (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Expression input (Vector2d (space @ units2)) =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  value .*. expression = constant value .*. expression

instance Multiplication' (Expression input (Qty units1)) (Vector2d (space @ units2)) where
  type
    Expression input (Qty units1) .*. Vector2d (space @ units2) =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  expression .*. vector = expression .*. vector2d vector

--- Qty-Direction2d ---
-----------------------

instance Multiplication' (Expression input (Qty units)) (Direction2d space) where
  type
    Expression input (Qty units) .*. Direction2d space =
      Expression input (Vector2d (space @ (units :*: Unitless)))
  scale .*. direction = scale .*. Vector2d.unit direction

--- Vector2d-Qty ---
--------------------

instance
  input1 ~ input2 =>
  Multiplication' (Expression input1 (Vector2d (space @ units1))) (Expression input2 (Qty units2))
  where
  type
    Expression input1 (Vector2d (space @ units1)) .*. Expression input2 (Qty units2) =
      Expression input1 (Vector2d (space @ (units1 :*: units2)))
  Vector2d x y .*. Qty scale =
    Vector2d (Expression1d.product x scale) (Expression1d.product y scale)

instance Multiplication' (Vector2d (space @ units1)) (Expression input (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Expression input (Qty units2) =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  vector .*. expression = vector2d vector .*. expression

instance Multiplication' (Expression input (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression input (Vector2d (space @ units1)) .*. Qty units2 =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  expression .*. value = expression .*. constant value

--- Direction2d-Qty ---
-----------------------

instance Multiplication' (Direction2d space) (Expression input (Qty units)) where
  type
    Direction2d space .*. Expression input (Qty units) =
      Expression input (Vector2d (space @ (Unitless :*: units)))
  direction .*. scale = Vector2d.unit direction .*. scale

----------------
--- DIVISION ---
----------------

--- Division instances ---
--------------------------

--- Qty-Qty ---
---------------

instance
  (input1 ~ input2, Units.Quotient units1 units2 units3) =>
  Division
    (Expression input1 (Qty units1))
    (Expression input2 (Qty units2))
    (Expression input1 (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Expression input (Qty units1)) (Qty units2) (Expression input (Qty units3))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Expression input (Qty units2)) (Expression input (Qty units3))

--- Vector2d-Qty ---
--------------------

instance
  (input1 ~ input2, Units.Quotient units1 units2 units3) =>
  Division
    (Expression input1 (Vector2d (space @ units1)))
    (Expression input2 (Qty units2))
    (Expression input1 (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector2d (space @ units1))
    (Expression input (Qty units2))
    (Expression input (Vector2d (space @ units3)))

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Expression input (Vector2d (space @ units1)))
    (Qty units2)
    (Expression input (Vector2d (space @ units3)))

---------------------------
--- Division' instances ---
---------------------------

--- Qty-Qty ---
---------------

instance
  input1 ~ input2 =>
  Division' (Expression input1 (Qty units1)) (Expression input2 (Qty units2))
  where
  type
    Expression input1 (Qty units1) ./. Expression input2 (Qty units2) =
      Expression input1 (Qty (units1 :/: units2))
  Qty lhs ./. Qty rhs = Qty (Expression1d.quotient lhs rhs)

instance Division' (Expression input (Qty units1)) (Qty units2) where
  type Expression input (Qty units1) ./. Qty units2 = Expression input (Qty (units1 :/: units2))
  expression ./. value = expression ./. constant value

instance Division' (Qty units1) (Expression input (Qty units2)) where
  type Qty units1 ./. Expression input (Qty units2) = Expression input (Qty (units1 :/: units2))
  value ./. expression = constant value ./. expression

--- Vector2d-Qty ---
--------------------

instance
  input1 ~ input2 =>
  Division' (Expression input1 (Vector2d (space @ units1))) (Expression input2 (Qty units2))
  where
  type
    Expression input1 (Vector2d (space @ units1)) ./. Expression input2 (Qty units2) =
      Expression input1 (Vector2d (space @ (units1 :/: units2)))
  Vector2d x y ./. Qty scale =
    Vector2d (Expression1d.quotient x scale) (Expression1d.quotient y scale)

instance Division' (Vector2d (space @ units1)) (Expression input (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Expression input (Qty units2) =
      Expression input (Vector2d (space @ (units1 :/: units2)))
  vector ./. expression = vector2d vector ./. expression

instance Division' (Expression input (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression input (Vector2d (space @ units1)) ./. Qty units2 =
      Expression input (Vector2d (space @ (units1 :/: units2)))
  expression ./. value = expression ./. constant value

-------------------
--- DOT PRODUCT ---
-------------------

--- DotMultiplication instances ---
-----------------------------------

instance
  (input1 ~ input2, space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  DotMultiplication
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Qty units3))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Expression input (Vector2d (space2 @ units)))
    (Expression input (Qty units))

instance
  space1 ~ space2 =>
  DotMultiplication
    (Expression input (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Expression input (Qty units))

--- DotMultiplication' instances ---
------------------------------------

instance
  (input1 ~ input2, space1 ~ space2) =>
  DotMultiplication'
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
  where
  type
    Expression input1 (Vector2d (space1 @ units1))
      .<>. Expression input2 (Vector2d (space2 @ units2)) =
      Expression input1 (Qty (units1 :*: units2))
  Vector2d x1 y1 .<>. Vector2d x2 y2 =
    Qty (Expression1d.sum (Expression1d.product x1 x2) (Expression1d.product y1 y2))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression input (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Expression input (Qty (units1 :*: units2))
  expression .<>. vector = expression .<>. vector2d vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Expression input (Vector2d (space2 @ units2)) =
      Expression input (Qty (units1 :*: units2))
  vector .<>. expression = vector2d vector .<>. expression

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression input (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Expression input (Vector2d (space1 @ units)) .<>. Direction2d space2 =
      Expression input (Qty (units :*: Unitless))
  expression .<>. direction = expression .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Expression input (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .<>. Expression input (Vector2d (space2 @ units)) =
      Expression input (Qty (Unitless :*: units))
  direction .<>. expression = Vector2d.unit direction .<>. expression

---------------------
--- CROSS PRODUCT ---
---------------------

--- CrossMultiplication instances ---
-------------------------------------

instance
  (input1 ~ input2, space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Qty units3))

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
  CrossMultiplication
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Qty units3))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Expression input (Vector2d (space2 @ units)))
    (Expression input (Qty units))

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Expression input (Vector2d (space1 @ units)))
    (Direction2d space2)
    (Expression input (Qty units))

--- CrossMultiplication' instances ---
--------------------------------------

instance
  (input1 ~ input2, space1 ~ space2) =>
  CrossMultiplication'
    (Expression input1 (Vector2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
  where
  type
    Expression input1 (Vector2d (space1 @ units1))
      .><. Expression input2 (Vector2d (space2 @ units2)) =
      Expression input1 (Qty (units1 :*: units2))
  Vector2d x1 y1 .><. Vector2d x2 y2 =
    Qty (Expression1d.difference (Expression1d.product x1 y2) (Expression1d.product y1 x2))

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression input (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Expression input (Qty (units1 :*: units2))
  expression .><. vector = expression .><. vector2d vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Expression input (Vector2d (space2 @ units2)) =
      Expression input (Qty (units1 :*: units2))
  vector .><. expression = vector2d vector .><. expression

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression input (Vector2d (space1 @ units)))
    (Direction2d space2)
  where
  type
    Expression input (Vector2d (space1 @ units)) .><. Direction2d space2 =
      Expression input (Qty (units :*: Unitless))
  expression .><. direction = expression .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Expression input (Vector2d (space2 @ units)))
  where
  type
    Direction2d space1 .><. Expression input (Vector2d (space2 @ units)) =
      Expression input (Qty (Unitless :*: units))
  direction .><. expression = Vector2d.unit direction .><. expression

-------------------
--- COMPOSITION ---
-------------------

instance
  Composition
    (Expression input Float)
    (Expression Float output)
    (Expression input output)
  where
  expression . Qty input =
    case expression of
      Qty x -> Qty (x . input)
      Point2d x y -> Point2d (x . input) (y . input)
      Vector2d x y -> Vector2d (x . input) (y . input)
      Point3d x y z -> Point3d (x . input) (y . input) (z . input)
      Vector3d x y z -> Vector3d (x . input) (y . input) (z . input)

instance
  Composition
    (Expression input Uv.Point)
    (Expression Uv.Point output)
    (Expression input output)
  where
  expression . Point2d uExpression vExpression = do
    let inputs = (uExpression, vExpression)
    case expression of
      Qty x -> Qty (x . inputs)
      Point2d x y -> Point2d (x . inputs) (y . inputs)
      Vector2d x y -> Vector2d (x . inputs) (y . inputs)
      Point3d x y z -> Point3d (x . inputs) (y . inputs) (z . inputs)
      Vector3d x y z -> Vector3d (x . inputs) (y . inputs) (z . inputs)

-----------------
--- FUNCTIONS ---
-----------------

zero :: Expression input (Qty units)
zero = constant Qty.zero

constant :: Qty units -> Expression input (Qty units)
constant value = Qty (Expression1d.constant (Units.coerce value))

parameter :: Expression Float Float
parameter = Qty Expression1d.parameter

u :: Expression Uv.Point Float
u = Qty Expression1d.u

v :: Expression Uv.Point Float
v = Qty Expression1d.v

squared' :: Expression input (Qty units) -> Expression input (Qty (units :*: units))
squared' (Qty expression) = Qty (Expression1d.squared expression)

squared ::
  Units.Squared units1 units2 =>
  Expression input (Qty units1) ->
  Expression input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Expression input (Qty (units :*: units)) -> Expression input (Qty units)
sqrt' (Qty expression) = Qty (Expression1d.sqrt expression)

sqrt ::
  Units.Squared units1 units2 =>
  Expression input (Qty units2) ->
  Expression input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Expression input Angle -> Expression input Float
sin (Qty expression) = Qty (Expression1d.sin expression)

cos :: Expression input Angle -> Expression input Float
cos (Qty expression) = Qty (Expression1d.cos expression)

interpolateFrom ::
  Expression input (Qty units) ->
  Expression input (Qty units) ->
  Expression input Float ->
  Expression input (Qty units)
interpolateFrom start end t = start + t * (end - start)

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
  curveDerivative (Qty expression) = Qty (Expression1d.curveDerivative expression)

instance
  SurfaceDerivative
    (Expression Uv.Point (Qty units))
    (Expression Uv.Point (Qty units))
  where
  surfaceDerivative p (Qty expression) = Qty (Expression1d.surfaceDerivative p expression)

instance
  CurveDerivative
    (Expression Float (Vector2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  curveDerivative (Vector2d x y) =
    Vector2d
      (Expression1d.curveDerivative x)
      (Expression1d.curveDerivative y)

instance
  CurveDerivative
    (Expression Float (Point2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  curveDerivative (Point2d x y) =
    Vector2d
      (Expression1d.curveDerivative x)
      (Expression1d.curveDerivative y)

instance
  SurfaceDerivative
    (Expression Uv.Point (Vector2d (space @ units)))
    (Expression Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (Vector2d x y) =
    Vector2d
      (Expression1d.surfaceDerivative p x)
      (Expression1d.surfaceDerivative p y)

instance
  SurfaceDerivative
    (Expression Uv.Point (Point2d (space @ units)))
    (Expression Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (Point2d x y) =
    Vector2d
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
  valueFunction (Qty expression) = do
    let f = curve1d_value_function (opensolid_curve1d_value_function (Expression1d.ptr expression))
    \(Qty.Qty x) -> Qty.Qty (f x)

instance BoundsFunction (Expression Float (Qty units)) (Range Unitless -> Range units) where
  boundsFunction (Qty expression) = do
    let f = curve1d_bounds_function (opensolid_curve1d_bounds_function (Expression1d.ptr expression))
    \(Range (Qty.Qty xLow) (Qty.Qty xHigh)) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f xLow xHigh outputs
      yLow <- Foreign.peekElemOff outputs 0
      yHigh <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty.Qty yLow) (Qty.Qty yHigh))

instance ValueFunction (Expression Uv.Point (Qty units)) (Uv.Point -> Qty units) where
  valueFunction (Qty expression) = do
    let f = surface1d_value_function (opensolid_surface1d_value_function (Expression1d.ptr expression))
    \(Point2d.Point2d (Qty.Qty x) (Qty.Qty y)) -> Qty.Qty (f x y)

instance BoundsFunction (Expression Uv.Point (Qty units)) (Uv.Bounds -> Range units) where
  boundsFunction (Qty expression) = do
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
  valueFunction (Point2d x y) = do
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
  boundsFunction (Point2d x y) = do
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
  valueFunction (Point2d x y) = do
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
  boundsFunction (Point2d x y) = do
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
