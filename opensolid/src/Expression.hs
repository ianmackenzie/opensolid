module Expression
  ( Expression (Point2d, Vector2d, Point3d, Vector3d)
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

import Angle qualified
import Bounds2d (Bounds2d (Bounds2d))
import Data.Int (Int64)
import Direction2d (Direction2d)
import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import IO qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Point3d (Point3d)
import Point3d qualified
import Qty (Qty (Qty))
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

data Expression input output where
  Parameter ::
    Expression Float Float
  U ::
    Expression Uv.Point Float
  V ::
    Expression Uv.Point Float
  Constant ::
    Qty units ->
    Expression input (Qty units)
  Coerce ::
    Expression input (Qty units1) ->
    Expression input (Qty units2)
  Negated ::
    Expression input (Qty units) ->
    Expression input (Qty units)
  Sum ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units)
  Difference ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units)
  Product' ::
    Expression input (Qty units1) ->
    Expression input (Qty units2) ->
    Expression input (Qty (units1 :*: units2))
  Quotient' ::
    Expression input (Qty units1) ->
    Expression input (Qty units2) ->
    Expression input (Qty (units1 :/: units2))
  Squared' ::
    Expression input (Qty units) ->
    Expression input (Qty (units :*: units))
  SquareRoot' ::
    Expression input (Qty (units :*: units)) ->
    Expression input (Qty units)
  Sine ::
    Expression input Angle ->
    Expression input Float
  Cosine ::
    Expression input Angle ->
    Expression input Float
  Point2d ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Point2d (space @ units))
  Vector2d ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Vector2d (space @ units))
  Point3d ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Point3d (space @ units))
  Vector3d ::
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Qty units) ->
    Expression input (Vector3d (space @ units))

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
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce expression) = Coerce expression
  coerce expression = Coerce expression

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Vector2d (space @ units1)))
    (Expression input2 (Vector2d (space @ units2)))
  where
  coerce (Vector2d vx vy) = Vector2d (Units.coerce vx) (Units.coerce vy)

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Vector3d (space @ units1)))
    (Expression input2 (Vector3d (space @ units2)))
  where
  coerce (Vector3d vx vy vz) = Vector3d (Units.coerce vx) (Units.coerce vy) (Units.coerce vz)

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Point2d (space @ units1)))
    (Expression input2 (Point2d (space @ units2)))
  where
  coerce (Point2d px py) = Point2d (Units.coerce px) (Units.coerce py)

instance
  input1 ~ input2 =>
  Units.Coercion
    (Expression input1 (Point3d (space @ units1)))
    (Expression input2 (Point3d (space @ units2)))
  where
  coerce (Point3d px py pz) = Point3d (Units.coerce px) (Units.coerce py) (Units.coerce pz)

----------------
--- NEGATION ---
----------------

instance Negation (Expression input (Qty units)) where
  negate (Constant value) = Constant (negate value)
  negate (Negated expression) = expression
  negate (Difference lhs rhs) = Difference rhs lhs
  negate expression = Negated expression

instance Negation (Expression input (Vector2d (space @ units))) where
  negate (Vector2d x y) = Vector2d (negate x) (negate y)

instance Negation (Expression input (Vector3d (space @ units))) where
  negate (Vector3d x y z) = Vector3d (negate x) (negate y) (negate z)

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
  Constant lhs + Constant rhs = Constant (lhs + rhs)
  lhs + Constant value | value == Qty.zero = lhs
  Constant value + rhs | value == Qty.zero = rhs
  lhs + rhs = Sum lhs rhs

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
  Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Vector2d (space1 @ units1)))
  where
  Vector2d x1 y1 + Vector2d.Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Vector2d (space1 @ units1)))
  where
  Vector2d.Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Vector3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Vector3d (space1 @ units1)))
  where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Vector3d (space1 @ units1)))
  where
  Vector3d x1 y1 z1 + Vector3d.Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Vector3d (space1 @ units1)))
  where
  Vector3d.Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Point2d (space1 @ units1)))
  where
  Point2d x1 y1 + Vector2d x2 y2 = Point2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Point2d (space1 @ units1)))
  where
  Point2d x1 y1 + Vector2d.Vector2d x2 y2 = Point2d (x1 + x2) (y1 + y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Point2d (space1 @ units1)))
  where
  Point2d.Point2d x1 y1 + Vector2d x2 y2 = Point2d (x1 + x2) (y1 + y2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input1 (Point3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Point3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 + Vector3d x2 y2 z2 = Point3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Expression input (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Point3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 + Vector3d.Vector3d x2 y2 z2 = Point3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Point3d (space1 @ units1)))
  where
  Point3d.Point3d x1 y1 z1 + Vector3d x2 y2 z2 = Point3d (x1 + x2) (y1 + y2) (z1 + z2)

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
  Constant lhs - Constant rhs = Constant (lhs - rhs)
  lhs - Constant value | value == Qty.zero = lhs
  Constant value - rhs | value == Qty.zero = negate rhs
  lhs - rhs = Difference lhs rhs

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
  Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Vector2d (space1 @ units1)))
  where
  Vector2d x1 y1 - Vector2d.Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Vector2d (space1 @ units1)))
  where
  Vector2d.Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Vector3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Vector3d (space1 @ units1)))
  where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Vector3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Vector3d (space1 @ units1)))
  where
  Vector3d x1 y1 z1 - Vector3d.Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Vector3d (space1 @ units1)))
  where
  Vector3d.Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Vector2d (space2 @ units2)))
    (Expression input1 (Point2d (space1 @ units1)))
  where
  Point2d x1 y1 - Vector2d x2 y2 = Point2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
    (Expression input (Point2d (space1 @ units1)))
  where
  Point2d x1 y1 - Vector2d.Vector2d x2 y2 = Point2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
    (Expression input (Point2d (space1 @ units1)))
  where
  Point2d.Point2d x1 y1 - Vector2d x2 y2 = Point2d (x1 - x2) (y1 - y2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point3d (space1 @ units1)))
    (Expression input2 (Vector3d (space2 @ units2)))
    (Expression input1 (Point3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 - Vector3d x2 y2 z2 = Point3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point3d (space1 @ units1)))
    (Vector3d (space2 @ units2))
    (Expression input (Point3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 - Vector3d.Vector3d x2 y2 z2 = Point3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression input (Vector3d (space2 @ units2)))
    (Expression input (Point3d (space1 @ units1)))
  where
  Point3d.Point3d x1 y1 z1 - Vector3d x2 y2 z2 = Point3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point2d (space1 @ units1)))
    (Expression input2 (Point2d (space2 @ units2)))
    (Expression input1 (Vector2d (space1 @ units1)))
  where
  Point2d x1 y1 - Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point2d (space1 @ units1)))
    (Point2d (space2 @ units2))
    (Expression input (Vector2d (space1 @ units1)))
  where
  Point2d x1 y1 - Point2d.Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Expression input (Point2d (space2 @ units2)))
    (Expression input (Vector2d (space1 @ units1)))
  where
  Point2d.Point2d x1 y1 - Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (input1 ~ input2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input1 (Point3d (space1 @ units1)))
    (Expression input2 (Point3d (space2 @ units2)))
    (Expression input1 (Vector3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Expression input (Point3d (space1 @ units1)))
    (Point3d (space2 @ units2))
    (Expression input (Vector3d (space1 @ units1)))
  where
  Point3d x1 y1 z1 - Point3d.Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Expression input (Point3d (space2 @ units2)))
    (Expression input (Vector3d (space1 @ units1)))
  where
  Point3d.Point3d x1 y1 z1 - Point3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

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
  Constant lhs .*. Constant rhs = Constant (lhs .*. rhs)
  _ .*. Constant value | value == Qty.zero = constant Qty.zero
  Constant value .*. _ | value == Qty.zero = constant Qty.zero
  lhs .*. Constant value | value == Qty.unit = Units.coerce lhs
  Constant value .*. rhs | value == Qty.unit = Units.coerce rhs
  lhs .*. rhs = Product' lhs rhs

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
  scale .*. Vector2d x y = Vector2d (scale .*. x) (scale .*. y)

instance Multiplication' (Qty units1) (Expression input (Vector2d (space @ units2))) where
  type
    Qty units1 .*. Expression input (Vector2d (space @ units2)) =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  scale .*. Vector2d x y = Vector2d (scale .*. x) (scale .*. y)

instance Multiplication' (Expression input (Qty units1)) (Vector2d (space @ units2)) where
  type
    Expression input (Qty units1) .*. Vector2d (space @ units2) =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  scale .*. Vector2d.Vector2d x y = Vector2d (scale .*. x) (scale .*. y)

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
  Vector2d x y .*. scale = Vector2d (x .*. scale) (y .*. scale)

instance Multiplication' (Vector2d (space @ units1)) (Expression input (Qty units2)) where
  type
    Vector2d (space @ units1) .*. Expression input (Qty units2) =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  Vector2d.Vector2d x y .*. scale = Vector2d (x .*. scale) (y .*. scale)

instance Multiplication' (Expression input (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression input (Vector2d (space @ units1)) .*. Qty units2 =
      Expression input (Vector2d (space @ (units1 :*: units2)))
  Vector2d x y .*. scale = Vector2d (x .*. scale) (y .*. scale)

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
  Constant lhs ./. Constant rhs = Constant (lhs ./. rhs)
  Constant value ./. _ | value == Qty.zero = constant Qty.zero
  lhs ./. Constant value = lhs ^*. (1.0 ./. value)
  lhs ./. rhs = Quotient' lhs rhs

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
  Vector2d x y ./. scale = Vector2d (x ./. scale) (y ./. scale)

instance Division' (Vector2d (space @ units1)) (Expression input (Qty units2)) where
  type
    Vector2d (space @ units1) ./. Expression input (Qty units2) =
      Expression input (Vector2d (space @ (units1 :/: units2)))
  Vector2d.Vector2d x y ./. scale = Vector2d (x ./. scale) (y ./. scale)

instance Division' (Expression input (Vector2d (space @ units1))) (Qty units2) where
  type
    Expression input (Vector2d (space @ units1)) ./. Qty units2 =
      Expression input (Vector2d (space @ (units1 :/: units2)))
  Vector2d x y ./. scale = Vector2d (x ./. scale) (y ./. scale)

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
  Vector2d x1 y1 .<>. Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression input (Vector2d (space1 @ units1)) .<>. Vector2d (space2 @ units2) =
      Expression input (Qty (units1 :*: units2))
  Vector2d x1 y1 .<>. Vector2d.Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .<>. Expression input (Vector2d (space2 @ units2)) =
      Expression input (Qty (units1 :*: units2))
  Vector2d.Vector2d x1 y1 .<>. Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

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
  Vector2d x1 y1 .><. Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Expression input (Vector2d (space1 @ units1)))
    (Vector2d (space2 @ units2))
  where
  type
    Expression input (Vector2d (space1 @ units1)) .><. Vector2d (space2 @ units2) =
      Expression input (Qty (units1 :*: units2))
  Vector2d x1 y1 .><. Vector2d.Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Expression input (Vector2d (space2 @ units2)))
  where
  type
    Vector2d (space1 @ units1) .><. Expression input (Vector2d (space2 @ units2)) =
      Expression input (Qty (units1 :*: units2))
  Vector2d.Vector2d x1 y1 .><. Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

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
  Coerce expression . inner = Coerce (expression . inner)
  Parameter . expression = expression
  Constant value . _ = Constant value
  Negated arg . inner = negate (arg . inner)
  Sum lhs rhs . inner = (lhs . inner) + (rhs . inner)
  Difference lhs rhs . inner = (lhs . inner) - (rhs . inner)
  Product' lhs rhs . inner = (lhs . inner) .*. (rhs . inner)
  Quotient' lhs rhs . inner = (lhs . inner) ./. (rhs . inner)
  Squared' arg . inner = squared' (arg . inner)
  SquareRoot' arg . inner = sqrt' (arg . inner)
  Sine arg . inner = sin (arg . inner)
  Cosine arg . inner = cos (arg . inner)
  Point2d x y . inner = Point2d (x . inner) (y . inner)
  Vector2d x y . inner = Vector2d (x . inner) (y . inner)
  Point3d x y z . inner = Point3d (x . inner) (y . inner) (z . inner)
  Vector3d x y z . inner = Vector3d (x . inner) (y . inner) (z . inner)

instance
  Composition
    (Expression input Uv.Point)
    (Expression Uv.Point output)
    (Expression input output)
  where
  Coerce expression . inner = Coerce (expression . inner)
  U . Point2d uCoordinate _ = uCoordinate
  V . Point2d _ vCoordinate = vCoordinate
  Constant value . _ = Constant value
  Negated arg . inner = negate (arg . inner)
  Sum lhs rhs . inner = (lhs . inner) + (rhs . inner)
  Difference lhs rhs . inner = (lhs . inner) - (rhs . inner)
  Product' lhs rhs . inner = (lhs . inner) .*. (rhs . inner)
  Quotient' lhs rhs . inner = (lhs . inner) ./. (rhs . inner)
  Squared' arg . inner = squared' (arg . inner)
  SquareRoot' arg . inner = sqrt' (arg . inner)
  Sine arg . inner = sin (arg . inner)
  Cosine arg . inner = cos (arg . inner)
  Point2d x y . inner = Point2d (x . inner) (y . inner)
  Vector2d x y . inner = Vector2d (x . inner) (y . inner)
  Point3d x y z . inner = Point3d (x . inner) (y . inner) (z . inner)
  Vector3d x y z . inner = Vector3d (x . inner) (y . inner) (z . inner)

-----------------
--- FUNCTIONS ---
-----------------

zero :: Expression input (Qty units)
zero = constant Qty.zero

constant :: Qty units -> Expression input (Qty units)
constant = Constant

parameter :: Expression Float Float
parameter = Parameter

u :: Expression Uv.Point Float
u = U

v :: Expression Uv.Point Float
v = V

squared' :: Expression input (Qty units) -> Expression input (Qty (units :*: units))
squared' (Constant expression) = Constant (Qty.squared' expression)
squared' (Negated expression) = squared' expression
squared' (SquareRoot' expression) = expression
squared' expression = Squared' expression

squared ::
  Units.Squared units1 units2 =>
  Expression input (Qty units1) ->
  Expression input (Qty units2)
squared = Units.specialize . squared'

sqrt' :: Expression input (Qty (units :*: units)) -> Expression input (Qty units)
sqrt' (Constant value) = Constant (Qty.sqrt' value)
sqrt' expression = SquareRoot' expression

sqrt ::
  Units.Squared units1 units2 =>
  Expression input (Qty units2) ->
  Expression input (Qty units1)
sqrt = sqrt' . Units.unspecialize

sin :: Expression input Angle -> Expression input Float
sin (Constant value) = Constant (Angle.sin value)
sin expression = Sine expression

cos :: Expression input Angle -> Expression input Float
cos (Constant value) = Constant (Angle.cos value)
cos (Negated expression) = cos expression
cos expression = Cosine expression

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
  curveDerivative expression = case expression of
    Constant _ -> zero
    Parameter -> constant 1.0
    Negated c -> negate (curveDerivative c)
    Sum c1 c2 -> curveDerivative c1 + curveDerivative c2
    Difference c1 c2 -> curveDerivative c1 - curveDerivative c2
    Product' c1 c2 -> curveDerivative c1 .*. c2 + c1 .*. curveDerivative c2
    Quotient' c1 c2 -> (curveDerivative c1 .*. c2 - c1 .*. curveDerivative c2) .!/.! squared' c2
    Squared' c -> 2.0 * c .*. curveDerivative c
    SquareRoot' c' -> curveDerivative c' .!/! (2.0 * sqrt' c')
    Sine c -> cos c * (curveDerivative c / Angle.radian)
    Cosine c -> negate (sin c) * (curveDerivative c / Angle.radian)
    Coerce c -> Units.coerce (curveDerivative c)

instance
  SurfaceDerivative
    (Expression Uv.Point (Qty units))
    (Expression Uv.Point (Qty units))
  where
  surfaceDerivative p expression = case expression of
    Constant _ -> zero
    U -> if p == Uv.U then constant 1.0 else zero
    V -> if p == Uv.V then constant 1.0 else zero
    Negated c -> negate (surfaceDerivative p c)
    Sum c1 c2 -> surfaceDerivative p c1 + surfaceDerivative p c2
    Difference c1 c2 -> surfaceDerivative p c1 - surfaceDerivative p c2
    Product' c1 c2 -> surfaceDerivative p c1 .*. c2 + c1 .*. surfaceDerivative p c2
    Quotient' c1 c2 -> (surfaceDerivative p c1 .*. c2 - c1 .*. surfaceDerivative p c2) .!/.! squared' c2
    Squared' c -> 2.0 * c .*. surfaceDerivative p c
    SquareRoot' c' -> surfaceDerivative p c' .!/! (2.0 * sqrt' c')
    Sine c -> cos c * (surfaceDerivative p c / Angle.radian)
    Cosine c -> negate (sin c) * (surfaceDerivative p c / Angle.radian)
    Coerce c -> Units.coerce (surfaceDerivative p c)

instance
  CurveDerivative
    (Expression Float (Vector2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  curveDerivative (Vector2d x y) = Vector2d (curveDerivative x) (curveDerivative y)

instance
  CurveDerivative
    (Expression Float (Point2d (space @ units)))
    (Expression Float (Vector2d (space @ units)))
  where
  curveDerivative (Point2d x y) = Vector2d (curveDerivative x) (curveDerivative y)

instance
  SurfaceDerivative
    (Expression Uv.Point (Vector2d (space @ units)))
    (Expression Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (Vector2d x y) = Vector2d (surfaceDerivative p x) (surfaceDerivative p y)

instance
  SurfaceDerivative
    (Expression Uv.Point (Point2d (space @ units)))
    (Expression Uv.Point (Vector2d (space @ units)))
  where
  surfaceDerivative p (Point2d x y) = Vector2d (surfaceDerivative p x) (surfaceDerivative p y)

-----------------
--- COMPILING ---
-----------------

data Expression#

type ExpressionPtr = Ptr Expression#

foreign import ccall unsafe "opensolid_expression_constant"
  opensolid_expression_constant :: Double -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_argument"
  opensolid_expression_argument :: Int64 -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_negate"
  opensolid_expression_negate :: ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_sum"
  opensolid_expression_sum :: ExpressionPtr -> ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_difference"
  opensolid_expression_difference :: ExpressionPtr -> ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_product"
  opensolid_expression_product :: ExpressionPtr -> ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_quotient"
  opensolid_expression_quotient :: ExpressionPtr -> ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_sqrt"
  opensolid_expression_sqrt :: ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_squared"
  opensolid_expression_squared :: ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_sin"
  opensolid_expression_sin :: ExpressionPtr -> ExpressionPtr

foreign import ccall unsafe "opensolid_expression_cos"
  opensolid_expression_cos :: ExpressionPtr -> ExpressionPtr

ptr :: Expression input (Qty units) -> ExpressionPtr
ptr expression = case expression of
  Coerce coerced -> ptr coerced
  Parameter -> opensolid_expression_argument (fromIntegral 0)
  U -> opensolid_expression_argument (fromIntegral 0)
  V -> opensolid_expression_argument (fromIntegral 1)
  Constant (Qty value) -> opensolid_expression_constant value
  Negated arg -> opensolid_expression_negate (ptr arg)
  Sum lhs rhs -> opensolid_expression_sum (ptr lhs) (ptr rhs)
  Difference lhs rhs -> opensolid_expression_difference (ptr lhs) (ptr rhs)
  Product' lhs rhs -> opensolid_expression_product (ptr lhs) (ptr rhs)
  Quotient' lhs rhs -> opensolid_expression_quotient (ptr lhs) (ptr rhs)
  Squared' arg -> opensolid_expression_squared (ptr arg)
  SquareRoot' arg -> opensolid_expression_sqrt (ptr arg)
  Sine arg -> opensolid_expression_sin (ptr arg)
  Cosine arg -> opensolid_expression_cos (ptr arg)

type Curve1dValueFunction = Double -> Double

type Curve1dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface1dValueFunction = Double -> Double -> Double

type Surface1dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

type Curve2dValueFunction = Double -> Ptr Double -> IO ()

type Curve2dBoundsFunction = Double -> Double -> Ptr Double -> IO ()

type Surface2dValueFunction = Double -> Double -> Ptr Double -> IO ()

type Surface2dBoundsFunction = Double -> Double -> Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "opensolid_curve1d_value_function"
  opensolid_curve1d_value_function :: ExpressionPtr -> FunPtr Curve1dValueFunction

foreign import ccall unsafe "opensolid_curve1d_bounds_function"
  opensolid_curve1d_bounds_function :: ExpressionPtr -> FunPtr Curve1dBoundsFunction

foreign import ccall unsafe "opensolid_surface1d_value_function"
  opensolid_surface1d_value_function :: ExpressionPtr -> FunPtr Surface1dValueFunction

foreign import ccall unsafe "opensolid_surface1d_bounds_function"
  opensolid_surface1d_bounds_function :: ExpressionPtr -> FunPtr Surface1dBoundsFunction

foreign import ccall unsafe "opensolid_curve2d_value_function"
  opensolid_curve2d_value_function :: ExpressionPtr -> ExpressionPtr -> FunPtr Curve2dValueFunction

foreign import ccall unsafe "opensolid_curve2d_bounds_function"
  opensolid_curve2d_bounds_function :: ExpressionPtr -> ExpressionPtr -> FunPtr Curve2dBoundsFunction

foreign import ccall unsafe "opensolid_surface2d_value_function"
  opensolid_surface2d_value_function :: ExpressionPtr -> ExpressionPtr -> FunPtr Surface2dValueFunction

foreign import ccall unsafe "opensolid_surface2d_bounds_function"
  opensolid_surface2d_bounds_function :: ExpressionPtr -> ExpressionPtr -> FunPtr Surface2dBoundsFunction

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
  valueFunction expression = do
    let f = curve1d_value_function (opensolid_curve1d_value_function (ptr expression))
    \(Qty x) -> Qty (f x)

instance BoundsFunction (Expression Float (Qty units)) (Range Unitless -> Range units) where
  boundsFunction expression = do
    let f = curve1d_bounds_function (opensolid_curve1d_bounds_function (ptr expression))
    \(Range (Qty xLow) (Qty xHigh)) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f xLow xHigh outputs
      yLow <- Foreign.peekElemOff outputs 0
      yHigh <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Range (Qty yLow) (Qty yHigh))

instance ValueFunction (Expression Uv.Point (Qty units)) (Uv.Point -> Qty units) where
  valueFunction expression = do
    let f = surface1d_value_function (opensolid_surface1d_value_function (ptr expression))
    \(Point2d.Point2d (Qty x) (Qty y)) -> Qty (f x y)

instance BoundsFunction (Expression Uv.Point (Qty units)) (Uv.Bounds -> Range units) where
  boundsFunction expression = do
    let expressionPtr = ptr expression
    let nativeFunction = opensolid_surface1d_bounds_function expressionPtr
    let wrappedFunction = surface1d_bounds_function nativeFunction
    let returnedFunction uvBounds = unsafeDupablePerformIO IO.do
          let Bounds2d uRange vRange = uvBounds
          let Range (Qty uLow) (Qty uHigh) = uRange
          let Range (Qty vLow) (Qty vHigh) = vRange
          outputs <- Alloc.mallocBytes 16
          wrappedFunction uLow uHigh vLow vHigh outputs
          low <- Foreign.peekElemOff outputs 0
          high <- Foreign.peekElemOff outputs 1
          Alloc.free outputs
          IO.succeed (Range (Qty low) (Qty high))
    returnedFunction

instance
  ValueFunction
    (Expression Float (Point2d (space @ units)))
    (Float -> Point2d (space @ units))
  where
  valueFunction (Point2d x y) = do
    let f = curve2d_value_function (opensolid_curve2d_value_function (ptr x) (ptr y))
    \(Qty tValue) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f tValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

instance
  BoundsFunction
    (Expression Float (Point2d (space @ units)))
    (Range Unitless -> Bounds2d (space @ units))
  where
  boundsFunction (Point2d x y) = do
    let f = curve2d_bounds_function (opensolid_curve2d_bounds_function (ptr x) (ptr y))
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
    (Expression Uv.Point (Point2d (space @ units)))
    (Uv.Point -> Point2d (space @ units))
  where
  valueFunction (Point2d x y) = do
    let f = surface2d_value_function (opensolid_surface2d_value_function (ptr x) (ptr y))
    \(Point2d.Point2d (Qty uValue) (Qty vValue)) -> unsafeDupablePerformIO IO.do
      outputs <- Alloc.mallocBytes 16
      f uValue vValue outputs
      px <- Foreign.peekElemOff outputs 0
      py <- Foreign.peekElemOff outputs 1
      Alloc.free outputs
      IO.succeed (Point2d.xy (Qty px) (Qty py))

instance
  BoundsFunction
    (Expression Uv.Point (Point2d (space @ units)))
    (Uv.Bounds -> Bounds2d (space @ units))
  where
  boundsFunction (Point2d x y) = do
    let f = surface2d_bounds_function (opensolid_surface2d_bounds_function (ptr x) (ptr y))
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
