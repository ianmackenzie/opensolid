module Volume1d.Function
  ( Function (Zero, Constant)
  , Interface (..)
  , evaluateAt
  , pointOn
  , segmentBounds
  , derivative
  , zero
  , constant
  , u
  , v
  , w
  , wrap
  , squared
  , sqrt
  , sin
  , cos
  )
where

import Angle qualified
import Bounds3d (Bounds3d)
import Bounds3d qualified
import Direction3d (Direction3d)
import Direction3d qualified
import OpenSolid
import Point3d (Point3d)
import Point3d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Units qualified
import Uvw qualified

class Show function => Interface function units | function -> units where
  evaluateAtImpl :: Point3d Uvw.Coordinates -> function -> Qty units
  segmentBoundsImpl :: Bounds3d Uvw.Coordinates -> function -> Range units
  derivativeImpl :: Direction3d Uvw.Space -> function -> Function units

data Function units where
  Function ::
    Interface function units =>
    function ->
    Function units
  Zero ::
    Function units
  Constant ::
    Qty units -> Function units
  U ::
    Function Unitless
  V ::
    Function Unitless
  W ::
    Function Unitless
  Negated ::
    Function units ->
    Function units
  Sum ::
    Function units ->
    Function units ->
    Function units
  Difference ::
    Function units ->
    Function units ->
    Function units
  Product ::
    Units.Product units1 units2 units3 =>
    Function units1 ->
    Function units2 ->
    Function units3
  Quotient ::
    Units.Quotient units1 units2 units3 =>
    Function units1 ->
    Function units2 ->
    Function units3
  Squared ::
    Units.Squared units1 units2 =>
    Function units1 ->
    Function units2
  SquareRoot ::
    Units.Squared units1 units2 =>
    Function units2 ->
    Function units1
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless

deriving instance Show (Function units)

instance
  ( units1 ~ units1'
  , units2 ~ units2'
  ) =>
  Units.Coercion
    units1
    units2
    (Function units1')
    (Function units2')

instance Negation (Function units) where
  negate Zero = Zero
  negate (Constant x) = Constant (negate x)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product f1 f2) = negate f1 * f2
  negate function = Negated function

instance Multiplication Sign (Function units) (Function units) where
  Positive * function = function
  Negative * function = -function

instance Multiplication (Function units) Sign (Function units) where
  function * Positive = function
  function * Negative = -function

instance units ~ units' => Addition (Function units) (Function units') (Function units) where
  Zero + function = function
  function + Zero = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance units ~ units' => Addition (Function units) (Qty units') (Function units) where
  function + value = function + constant value

instance units ~ units' => Addition (Qty units) (Function units') (Function units) where
  value + function = constant value + function

instance units ~ units' => Subtraction (Function units) (Function units') (Function units) where
  Zero - function = negate function
  function - Zero = function
  Constant x - Constant y = constant (x - y)
  function1 - function2 = Difference function1 function2

instance units ~ units' => Subtraction (Function units) (Qty units') (Function units) where
  function - value = function - constant value

instance units ~ units' => Subtraction (Qty units) (Function units') (Function units) where
  value - function = constant value - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero * _ = Zero
  _ * Zero = Zero
  Constant x * Constant y = Constant (x * y)
  Constant x * function | Units.drop x == 1.0 = Units.add (Units.drop function)
  Constant x * function | Units.drop x == -1.0 = Units.add (Units.drop (negate function))
  Constant x * Negated c = negate x * c
  f1 * (Constant x) = Constant x * f1
  Constant x * Product (Constant y) c =
    Units.add (Product (Constant (Units.drop x * Units.drop y)) (Units.drop c))
  function1 * function2 = Product function1 function2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function * value = function * constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value * function = constant value * function

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero / _ = Zero
  Constant x / Constant y = Constant (x / y)
  function / Constant x = Units.specialize do
    (Units.generalize 1.0 ./ Units.generalize x) .* Units.generalize function
  function1 / function2 = Quotient function1 function2

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function / value = function / constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value / function = constant value / function

evaluateAt :: Point3d Uvw.Coordinates -> Function units -> Qty units
evaluateAt uvw function =
  case function of
    Function f -> evaluateAtImpl uvw f
    Zero -> Qty.zero
    Constant x -> x
    U -> Point3d.xCoordinate uvw
    V -> Point3d.yCoordinate uvw
    W -> Point3d.zCoordinate uvw
    Negated f -> negate (evaluateAt uvw f)
    Sum f1 f2 -> evaluateAt uvw f1 + evaluateAt uvw f2
    Difference f1 f2 -> evaluateAt uvw f1 - evaluateAt uvw f2
    Product f1 f2 -> evaluateAt uvw f1 * evaluateAt uvw f2
    Quotient f1 f2 -> evaluateAt uvw f1 / evaluateAt uvw f2
    Squared f -> Qty.squared (evaluateAt uvw f)
    SquareRoot f -> Qty.sqrt (evaluateAt uvw f)
    Sin f -> Angle.sin (evaluateAt uvw f)
    Cos f -> Angle.cos (evaluateAt uvw f)

pointOn :: Function units -> Point3d Uvw.Coordinates -> Qty units
pointOn function uvw = evaluateAt uvw function

segmentBounds :: Bounds3d Uvw.Coordinates -> Function units -> Range units
segmentBounds uvw function =
  case function of
    Function f -> segmentBoundsImpl uvw f
    Zero -> Range.constant Qty.zero
    Constant x -> Range.constant x
    U -> Bounds3d.xCoordinate uvw
    V -> Bounds3d.yCoordinate uvw
    W -> Bounds3d.zCoordinate uvw
    Negated f -> negate (segmentBounds uvw f)
    Sum f1 f2 -> segmentBounds uvw f1 + segmentBounds uvw f2
    Difference f1 f2 -> segmentBounds uvw f1 - segmentBounds uvw f2
    Product f1 f2 -> segmentBounds uvw f1 * segmentBounds uvw f2
    Quotient f1 f2 -> segmentBounds uvw f1 / segmentBounds uvw f2
    Squared f -> Range.squared (segmentBounds uvw f)
    SquareRoot f -> Range.sqrt (segmentBounds uvw f)
    Sin f -> Range.sin (segmentBounds uvw f)
    Cos f -> Range.cos (segmentBounds uvw f)

derivative :: Direction3d Uvw.Space -> Function units -> Function units
derivative direction function =
  case function of
    Function f -> derivativeImpl direction f
    Zero -> zero
    Constant _ -> zero
    U -> constant (Direction3d.xComponent direction)
    V -> constant (Direction3d.yComponent direction)
    W -> constant (Direction3d.zComponent direction)
    Negated f -> negate (derivative direction f)
    Sum f1 f2 -> derivative direction f1 + derivative direction f2
    Difference f1 f2 -> derivative direction f1 - derivative direction f2
    Product f1 f2 -> derivative direction f1 * f2 + f1 * derivative direction f2
    Quotient f1 f2 -> Units.specialize do
      let f1' = Units.generalize f1
      let f2' = Units.generalize f2
      (derivative direction f1' .* f2' - f1' .* derivative direction f2') ./ squared f2'
    Squared f -> 2.0 * f * derivative direction f
    SquareRoot f -> derivative direction f / (2.0 * sqrt f)
    Sin f -> cos f * Units.drop (derivative direction f)
    Cos f -> negate (sin f) * Units.drop (derivative direction f)

zero :: Function units
zero = Zero

constant :: Qty units -> Function units
constant value = if value == Qty.zero then Zero else Constant value

u :: Function Unitless
u = U

v :: Function Unitless
v = V

w :: Function Unitless
w = W

wrap :: Interface function units => function -> Function units
wrap = Function

squared :: Units.Squared units1 units2 => Function units1 -> Function units2
squared Zero = Zero
squared (Constant x) = Constant (x * x)
squared (Negated f) = squared f
squared (Cos f) = Units.add (cosSquared f)
squared (Sin f) = Units.add (sinSquared f)
squared function = Squared function

cosSquared :: Function Radians -> Function Unitless
cosSquared f = 0.5 * cos (2.0 * f) + 0.5

sinSquared :: Function Radians -> Function Unitless
sinSquared f = 0.5 - 0.5 * cos (2.0 * f)

sqrt :: Units.Squared units1 units2 => Function units2 -> Function units1
sqrt Zero = Zero
sqrt (Constant x) = Constant (Qty.sqrt x)
sqrt function = SquareRoot function

sin :: Function Radians -> Function Unitless
sin Zero = Zero
sin (Constant x) = constant (Angle.sin x)
sin function = Sin function

cos :: Function Radians -> Function Unitless
cos Zero = Constant 1.0
cos (Constant x) = constant (Angle.cos x)
cos function = Cos function
