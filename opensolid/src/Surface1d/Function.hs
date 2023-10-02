module Surface1d.Function
  ( Function
  , Operations
  , evaluateAt
  , pointOn
  , segmentBounds
  , derivative
  , zero
  , constant
  , u
  , v
  )
where

import Angle qualified
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import CoordinateSystem (UvCoordinates, UvSpace)
import Direction2d (Direction2d)
import Direction2d qualified
import Generic qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Units qualified

class (Show function) => Operations function units | function -> units where
  evaluateAtImpl :: Point2d UvCoordinates -> function -> Qty units
  segmentBoundsImpl :: BoundingBox2d UvCoordinates -> function -> Range units
  derivativeImpl :: Direction2d UvSpace -> function -> Function units

data Function units where
  Function ::
    forall function units.
    (Operations function units) =>
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
    forall units1 units2 units3.
    (Units.Product units1 units2 units3) =>
    Function units1 ->
    Function units2 ->
    Function units3
  Quotient ::
    forall units1 units2 units3.
    (Units.Quotient units1 units2 units3) =>
    Function units1 ->
    Function units2 ->
    Function units3
  Squared ::
    forall units1 units2.
    (Units.Squared units1 units2) =>
    Function units1 ->
    Function units2
  SquareRoot ::
    forall units1 units2.
    (Units.Squared units1 units2) =>
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

instance Generic.HasZero (Function units) where
  zeroImpl = zero

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

instance (units ~ units') => Addition (Function units) (Function units') (Function units) where
  Zero + function = function
  function + Zero = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance (units ~ units') => Addition (Function units) (Qty units') (Function units) where
  function + value = function + constant value

instance (units ~ units') => Addition (Qty units) (Function units') (Function units) where
  value + function = constant value + function

instance (units ~ units') => Subtraction (Function units) (Function units') (Function units) where
  Zero - function = negate function
  function - Zero = function
  Constant x - Constant y = constant (x - y)
  function1 - function2 = Difference function1 function2

instance (units ~ units') => Subtraction (Function units) (Qty units') (Function units) where
  function - value = function - constant value

instance (units ~ units') => Subtraction (Qty units) (Function units') (Function units) where
  value - function = constant value - function

instance
  (Units.Product units1 units2 units3) =>
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
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function * value = function * constant value

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value * function = constant value * function

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero / _ = Zero
  Constant x / Constant y = Constant (x / y)
  function / Constant x =
    Units.specialize $
      (Units.generalize 1.0 ./ Units.generalize x) .* Units.generalize function
  function1 / function2 = Quotient function1 function2

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function / value = function / constant value

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value / function = constant value / function

evaluateAt :: Point2d UvCoordinates -> Function units -> Qty units
evaluateAt uv function =
  case function of
    Function f -> evaluateAtImpl uv f
    Zero -> Qty.zero
    Constant x -> x
    U -> Point2d.xCoordinate uv
    V -> Point2d.yCoordinate uv
    Negated f -> negate (evaluateAt uv f)
    Sum f1 f2 -> evaluateAt uv f1 + evaluateAt uv f2
    Difference f1 f2 -> evaluateAt uv f1 - evaluateAt uv f2
    Product f1 f2 -> evaluateAt uv f1 * evaluateAt uv f2
    Quotient f1 f2 -> evaluateAt uv f1 / evaluateAt uv f2
    Squared f -> Qty.squared (evaluateAt uv f)
    SquareRoot f -> Qty.sqrt (evaluateAt uv f)
    Sin f -> Angle.sin (evaluateAt uv f)
    Cos f -> Angle.cos (evaluateAt uv f)

pointOn :: Function units -> Point2d UvCoordinates -> Qty units
pointOn function uv = evaluateAt uv function

segmentBounds :: BoundingBox2d UvCoordinates -> Function units -> Range units
segmentBounds uv function =
  case function of
    Function f -> segmentBoundsImpl uv f
    Zero -> Range.constant Qty.zero
    Constant x -> Range.constant x
    U -> BoundingBox2d.xCoordinate uv
    V -> BoundingBox2d.yCoordinate uv
    Negated f -> negate (segmentBounds uv f)
    Sum f1 f2 -> segmentBounds uv f1 + segmentBounds uv f2
    Difference f1 f2 -> segmentBounds uv f1 - segmentBounds uv f2
    Product f1 f2 -> segmentBounds uv f1 * segmentBounds uv f2
    Quotient f1 f2 -> segmentBounds uv f1 / segmentBounds uv f2
    Squared f -> Range.squared (segmentBounds uv f)
    SquareRoot f -> Range.sqrt (segmentBounds uv f)
    Sin f -> Range.sin (segmentBounds uv f)
    Cos f -> Range.cos (segmentBounds uv f)

derivative :: Direction2d UvSpace -> Function units -> Function units
derivative direction function =
  case function of
    Function f -> derivativeImpl direction f
    Zero -> zero
    Constant _ -> zero
    U -> constant (Direction2d.xComponent direction)
    V -> constant (Direction2d.yComponent direction)
    Negated f -> negate (derivative direction f)
    Sum f1 f2 -> derivative direction f1 + derivative direction f2
    Difference f1 f2 -> derivative direction f1 - derivative direction f2
    Product f1 f2 -> derivative direction f1 * f2 + f1 * derivative direction f2
    Quotient f1 f2 ->
      let f1' = Units.generalize f1
          f2' = Units.generalize f2
       in Units.specialize ((derivative direction f1' .* f2' - f1' .* derivative direction f2') ./ squared f2')
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

squared :: (Units.Squared units1 units2) => Function units1 -> Function units2
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

sqrt :: (Units.Squared units1 units2) => Function units2 -> Function units1
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
