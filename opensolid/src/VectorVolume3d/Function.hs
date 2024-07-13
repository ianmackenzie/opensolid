module VectorVolume3d.Function
  ( Function
  , Interface (..)
  , evaluateAt
  , pointOn
  , segmentBounds
  , derivative
  , zero
  , constant
  , new
  , xyz
  )
where

import Bounds3d (Bounds3d)
import Direction3d (Direction3d)
import Float qualified
import OpenSolid
import Point3d (Point3d)
import Units qualified
import Uvw qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified
import Volume1d qualified
import Volume1d.Function qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateAtImpl :: Point3d Uvw.Coordinates -> function -> Vector3d coordinateSystem
  segmentBoundsImpl :: Bounds3d Uvw.Coordinates -> function -> VectorBounds3d coordinateSystem
  derivativeImpl :: Direction3d Uvw.Space -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Zero ::
    Function (space @ units)
  Constant ::
    Vector3d (space @ units) -> Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  XYZ ::
    Volume1d.Function units ->
    Volume1d.Function units ->
    Volume1d.Function units ->
    Function (space @ units)
  Negated ::
    Function (space @ units) ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Product1d3d ::
    Volume1d.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product3d1d ::
    Function (space @ units1) ->
    Volume1d.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient ::
    Function (space @ units1) ->
    Volume1d.Function units2 ->
    Function (space @ (units1 :/: units2))

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units
  type Erase (Function (space @ units)) = Function (space @ Unitless)

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce Zero = Zero
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance Negation (Function (space @ units)) where
  negate Zero = Zero
  negate (Constant x) = Constant (negate x)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product1d3d f1 f2) = negate f1 .*. f2
  negate (Product3d1d f1 f2) = f1 .*. negate f2
  negate function = Negated function

instance Multiplication Sign (Function (space @ units)) (Function (space @ units))

instance Multiplication' Sign (Function (space @ units)) where
  type Sign .*. Function (space @ units) = Function (space @ (Unitless :*: units))
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function (space @ units)) Sign (Function (space @ units))

instance Multiplication' (Function (space @ units)) Sign where
  type Function (space @ units) .*. Sign = Function (space @ (units :*: Unitless))
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  Zero + function = function
  function + Zero = function
  Constant v1 + Constant v2 = constant (v1 + v2)
  function1 + function2 = Sum function1 function2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  function + vector = function + constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  value + function = constant value + function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  Zero - function = negate function
  function - Zero = function
  Constant v1 - Constant v2 = constant (v1 - v2)
  function1 - function2 = Difference function1 function2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  function - vector = function - constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  vector - function = constant vector - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Volume1d.Function units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Volume1d.Function units1) (Function (space @ units2)) where
  type
    Volume1d.Function units1 .*. Function (space @ units2) =
      Function (space @ (units1 :*: units2))
  Volume1d.Function.Zero .*. _ = Zero
  _ .*. Zero = Zero
  Volume1d.Function.Constant a .*. Constant b = Constant (a .*. b)
  f1 .*. f2 = Product1d3d f1 f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Volume1d.Function units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Volume1d.Function units2) where
  type
    Function (space @ units1) .*. Volume1d.Function units2 =
      Function (space @ (units1 :*: units2))
  Zero .*. _ = Zero
  _ .*. Volume1d.Function.Zero = Zero
  Constant a .*. Volume1d.Function.Constant b = Constant (a .*. b)
  f1 .*. f2 = Product3d1d f1 f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Qty units2) where
  type Function (space @ units1) .*. Qty units2 = Function (space @ (units1 :*: units2))
  function .*. value = function .*. Volume1d.Function.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Qty units1) (Function (space @ units2)) where
  type Qty units1 .*. Function (space @ units2) = Function (space @ (units1 :*: units2))
  value .*. function = Volume1d.Function.constant value .*. function

instance Multiplication' (Function (space @ units)) Int where
  type Function (space @ units) .*. Int = Function (space @ (units :*: Unitless))
  function .*. scale = function .*. Float.int scale

instance Multiplication' Int (Function (space @ units)) where
  type Int .*. Function (space @ units) = Function (space @ (Unitless :*: units))
  scale .*. function = Float.int scale .*. function

instance Multiplication (Function (space @ units)) Int (Function (space @ units))

instance Multiplication Int (Function (space @ units)) (Function (space @ units))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Volume1d.Function units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Volume1d.Function units2) where
  type
    Function (space @ units1) ./. Volume1d.Function units2 =
      Function (space @ (units1 :/: units2))
  Zero ./. _ = Zero
  Constant a ./. Volume1d.Function.Constant b = Constant (a ./. b)
  function ./. Volume1d.Function.Constant x = (1 ./. x) .*^ function
  function1 ./. function2 = Quotient function1 function2

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Qty units2) where
  type Function (space @ units1) ./. Qty units2 = Function (space @ (units1 :/: units2))
  function ./. value = function ./. Volume1d.Function.constant value

instance Division' (Function (space @ units)) Int where
  type Function (space @ units) ./. Int = Function (space @ (units :/: Unitless))
  function ./. scale = function ./. Float.int scale

instance Division (Function (space @ units)) Int (Function (space @ units))

evaluateAt :: Point3d Uvw.Coordinates -> Function (space @ units) -> Vector3d (space @ units)
evaluateAt uv function =
  case function of
    Function f -> evaluateAtImpl uv f
    Zero -> Vector3d.zero
    Constant v -> v
    Coerce f -> Units.coerce (evaluateAt uv f)
    XYZ x y z ->
      Vector3d.xyz
        (Volume1d.Function.evaluateAt uv x)
        (Volume1d.Function.evaluateAt uv y)
        (Volume1d.Function.evaluateAt uv z)
    Negated f -> negate (evaluateAt uv f)
    Sum f1 f2 -> evaluateAt uv f1 + evaluateAt uv f2
    Difference f1 f2 -> evaluateAt uv f1 - evaluateAt uv f2
    Product1d3d f1 f2 -> Volume1d.Function.evaluateAt uv f1 .*. evaluateAt uv f2
    Product3d1d f1 f2 -> evaluateAt uv f1 .*. Volume1d.Function.evaluateAt uv f2
    Quotient f1 f2 -> evaluateAt uv f1 ./. Volume1d.Function.evaluateAt uv f2

pointOn :: Function (space @ units) -> Point3d Uvw.Coordinates -> Vector3d (space @ units)
pointOn function uv = evaluateAt uv function

segmentBounds ::
  Bounds3d Uvw.Coordinates ->
  Function (space @ units) ->
  VectorBounds3d (space @ units)
segmentBounds uv function =
  case function of
    Function f -> segmentBoundsImpl uv f
    Zero -> VectorBounds3d.constant Vector3d.zero
    Constant v -> VectorBounds3d.constant v
    Coerce f -> Units.coerce (segmentBounds uv f)
    XYZ x y z ->
      VectorBounds3d.xyz
        (Volume1d.Function.segmentBounds uv x)
        (Volume1d.Function.segmentBounds uv y)
        (Volume1d.Function.segmentBounds uv z)
    Negated f -> negate (segmentBounds uv f)
    Sum f1 f2 -> segmentBounds uv f1 + segmentBounds uv f2
    Difference f1 f2 -> segmentBounds uv f1 - segmentBounds uv f2
    Product1d3d f1 f2 -> Volume1d.Function.segmentBounds uv f1 .*. segmentBounds uv f2
    Product3d1d f1 f2 -> segmentBounds uv f1 .*. Volume1d.Function.segmentBounds uv f2
    Quotient f1 f2 -> segmentBounds uv f1 ./. Volume1d.Function.segmentBounds uv f2

derivative :: Direction3d Uvw.Space -> Function units -> Function units
derivative direction function =
  case function of
    Function f -> derivativeImpl direction f
    Zero -> zero
    Constant _ -> zero
    Coerce f -> Units.coerce (derivative direction f)
    XYZ x y z ->
      XYZ
        (Volume1d.Function.derivative direction x)
        (Volume1d.Function.derivative direction y)
        (Volume1d.Function.derivative direction z)
    Negated f -> negate (derivative direction f)
    Sum f1 f2 -> derivative direction f1 + derivative direction f2
    Difference f1 f2 -> derivative direction f1 - derivative direction f2
    Product1d3d f1 f2 ->
      Volume1d.Function.derivative direction f1 .*. f2 + f1 .*. derivative direction f2
    Product3d1d f1 f2 ->
      derivative direction f1 .*. f2 + f1 .*. Volume1d.Function.derivative direction f2
    Quotient f1 f2 ->
      (derivative direction f1 .*. f2 - f1 .*. Volume1d.Function.derivative direction f2)
        .!/.! Volume1d.Function.squared' f2

zero :: Function (space @ units)
zero = Zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant vector = if vector == Vector3d.zero then Zero else Constant vector

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

xyz ::
  Volume1d.Function units ->
  Volume1d.Function units ->
  Volume1d.Function units ->
  Function (space @ units)
xyz = XYZ
