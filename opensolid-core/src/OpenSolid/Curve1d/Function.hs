module OpenSolid.Curve1d.Function
  ( Function (Parametric)
  , Zero
  , Interface (..)
  , evaluate
  , evaluateBounds
  , derivative
  , new
  , zero
  , constant
  , parametric
  , t
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  , reverse
  , integral
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Composition
import OpenSolid.Curve1d.Function.Integral (Integral (Integral))
import OpenSolid.Curve1d.Zero (Zero)
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Units (Meters, Radians, SquareMeters)
import OpenSolid.Units qualified as Units

class
  Show function =>
  Interface function units
    | function -> units
  where
  evaluateImpl :: function -> Float -> Qty units
  evaluateBoundsImpl :: function -> Range Unitless -> Range units
  derivativeImpl :: function -> Function units

data Function units where
  Function ::
    Interface function units =>
    function ->
    Function units
  Parametric ::
    Expression Float (Qty units) ->
    Function units
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
  Product' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :*: units2)
  Quotient' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :/: units2)
  Squared' ::
    Function units ->
    Function (units :*: units)
  SquareRoot' ::
    Function (units :*: units) ->
    Function units
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless
  Coerce ::
    Function units1 ->
    Function units2
  Reversed ::
    Function units ->
    Function units

deriving instance Show (Function units)

instance FFI (Function Unitless) where
  representation = FFI.classRepresentation "CurveFunction"

instance FFI (Function Meters) where
  representation = FFI.classRepresentation "LengthCurveFunction"

instance FFI (Function SquareMeters) where
  representation = FFI.classRepresentation "AreaCurveFunction"

instance FFI (Function Radians) where
  representation = FFI.classRepresentation "AngleCurveFunction"

instance HasUnits (Function units) where
  type UnitsOf (Function units) = units

instance Units.Coercion (Function unitsA) (Function unitsB) where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance
  units1 ~ units2 =>
  ApproximateEquality (Function units1) (Function units2) units1
  where
  function1 ~= function2 = function1 - function2 ~= Qty.zero

instance
  units1 ~ units2 =>
  ApproximateEquality (Function units1) (Qty units2) units1
  where
  function ~= value = List.allTrue [evaluate function tValue ~= value | tValue <- Parameter.samples]

instance Interface (Function units) units where
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative

new :: Interface function units => function -> Function units
new = Function

-- | A function equal to zero everywhere.
zero :: Function units
zero = constant Qty.zero

-- | Create a function with the given constant value.
constant :: Qty units -> Function units
constant = Parametric . Expression.constant

parametric :: Expression Float (Qty units) -> Function units
parametric = Parametric

{-| A function parameter.

In other words, a function whose value is equal to its input parameter.
When defining parametric functions, you will typically start with 'Function.t'
and then use arithmetic operators etc. to build up more complex functions.
-}
t :: Function Unitless
t = Parametric Expression.t

instance Negation (Function units) where
  negate (Parametric expression) = Parametric -expression
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate function = Negated function

instance Multiplication Sign (Function units) (Function units)

instance Multiplication' Sign (Function units) where
  type Sign .*. Function units = Function (Unitless :*: units)
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function units) Sign (Function units)

instance Multiplication' (Function units) Sign where
  type Function units .*. Sign = Function (units :*: Unitless)
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance units ~ units_ => Addition (Function units) (Function units_) (Function units) where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

instance units ~ units_ => Addition (Function units) (Qty units_) (Function units) where
  function + value = function + constant value

instance units ~ units_ => Addition (Qty units) (Function units_) (Function units) where
  value + function = constant value + function

instance
  units1 ~ units2 =>
  Subtraction (Function units1) (Function units2) (Function units1)
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance
  units1 ~ units2 =>
  Subtraction (Function units1) (Qty units2) (Function units1)
  where
  function - value = function - constant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Function units2) (Function units1)
  where
  value - function = constant value - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function units1) (Function units2) (Function units3)

instance Multiplication' (Function units1) (Function units2) where
  type Function units1 .*. Function units2 = Function (units1 :*: units2)
  Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function units1) (Qty units2) (Function units3)

instance Multiplication' (Function units1) (Qty units2) where
  type Function units1 .*. Qty units2 = Function (units1 :*: units2)
  function .*. value = function .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function units2) (Function units3)

instance Multiplication' (Qty units1) (Function units2) where
  type Qty units1 .*. Function units2 = Function (units1 :*: units2)
  value .*. function = constant value .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Division' (Function units1) (Function units2) where
  type Function units1 ./. Function units2 = Function (units1 :/: units2)
  Parametric lhs ./. Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Qty units2) (Function units3)

instance Division' (Function units1) (Qty units2) where
  type Function units1 ./. Qty units2 = Function (units1 :/: units2)
  function ./. value = function ./. constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function units2) (Function units3)

instance Division' (Qty units1) (Function units2) where
  type Qty units1 ./. Function units2 = Function (units1 :/: units2)
  value ./. function = constant value ./. function

instance Composition (Function Unitless) (Function units) (Function units) where
  Parametric outer . Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance Interface (Function units :.: Function Unitless) units where
  evaluateImpl (outer :.: inner) tValue =
    evaluate outer (evaluate inner tValue)
  evaluateBoundsImpl (outer :.: inner) tRange =
    evaluateBounds outer (evaluateBounds inner tRange)
  derivativeImpl (outer :.: inner) =
    (derivative outer . inner) * derivative inner

{-| Evaluate a function at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: Function units -> Float -> Qty units
evaluate function tValue = case function of
  Function f -> evaluateImpl f tValue
  Parametric expression -> Expression.evaluate expression tValue
  Negated f -> negate (evaluate f tValue)
  Sum f1 f2 -> evaluate f1 tValue + evaluate f2 tValue
  Difference f1 f2 -> evaluate f1 tValue - evaluate f2 tValue
  Product' f1 f2 -> evaluate f1 tValue .*. evaluate f2 tValue
  Quotient' f1 f2 -> evaluate f1 tValue ./. evaluate f2 tValue
  Squared' f -> Qty.squared' (evaluate f tValue)
  SquareRoot' f' -> Qty.sqrt' (evaluate f' tValue)
  Sin f -> Angle.sin (evaluate f tValue)
  Cos f -> Angle.cos (evaluate f tValue)
  Coerce f -> Units.coerce (evaluate f tValue)
  Reversed f -> evaluate f (1.0 - tValue)

evaluateBounds :: Function units -> Range Unitless -> Range units
evaluateBounds function tRange = case function of
  Function f -> evaluateBoundsImpl f tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Negated f -> negate (evaluateBounds f tRange)
  Sum f1 f2 -> evaluateBounds f1 tRange + evaluateBounds f2 tRange
  Difference f1 f2 -> evaluateBounds f1 tRange - evaluateBounds f2 tRange
  Product' f1 f2 -> evaluateBounds f1 tRange .*. evaluateBounds f2 tRange
  Quotient' f1 f2 -> evaluateBounds f1 tRange ./. evaluateBounds f2 tRange
  Squared' f -> Range.squared' (evaluateBounds f tRange)
  SquareRoot' f' -> Range.sqrt' (evaluateBounds f' tRange)
  Sin f -> Range.sin (evaluateBounds f tRange)
  Cos f -> Range.cos (evaluateBounds f tRange)
  Coerce f -> Units.coerce (evaluateBounds f tRange)
  Reversed f -> evaluateBounds f (1.0 - tRange)

derivative :: Function units -> Function units
derivative function = case function of
  Function f -> derivativeImpl f
  Parametric expression -> Parametric (Expression.curveDerivative expression)
  Negated f -> negate (derivative f)
  Sum f1 f2 -> derivative f1 + derivative f2
  Difference f1 f2 -> derivative f1 - derivative f2
  Product' f1 f2 -> derivative f1 .*. f2 + f1 .*. derivative f2
  Quotient' f1 f2 -> (derivative f1 .*. f2 - f1 .*. derivative f2) .!/.! squared' f2
  Squared' f -> 2.0 * f .*. derivative f
  SquareRoot' f' -> derivative f' .!/! (2.0 * sqrt' f')
  Sin f -> cos f * (derivative f / Angle.radian)
  Cos f -> negate (sin f) * (derivative f / Angle.radian)
  Coerce f -> Units.coerce (derivative f)
  Reversed f -> -(reverse (derivative f))

reverse :: Function units -> Function units
reverse (Parametric expression) = Parametric (expression . Expression.r)
reverse function = new (Reversed function)

-- | Compute the square of a function.
squared :: Units.Squared units1 units2 => Function units1 -> Function units2
squared function = Units.specialize (squared' function)

squared' :: Function units -> Function (units :*: units)
squared' (Parametric expression) = Parametric (Expression.squared' expression)
squared' (Negated function) = squared' function
squared' function = Squared' function

-- | Compute the square root of a function.
sqrt :: Units.Squared units1 units2 => Function units2 -> Function units1
sqrt function = sqrt' (Units.unspecialize function)

sqrt' :: Function (units :*: units) -> Function units
sqrt' (Parametric expression) = Parametric (Expression.sqrt' expression)
sqrt' function = SquareRoot' function

-- | Compute the sine of a function.
sin :: Function Radians -> Function Unitless
sin (Parametric expression) = Parametric (Expression.sin expression)
sin function = Sin function

-- | Compute the cosine of a function.
cos :: Function Radians -> Function Unitless
cos (Parametric expression) = Parametric (Expression.cos expression)
cos function = Cos function

integral :: Function units -> Estimate units
integral function = Estimate.new (Integral function (derivative function) Range.unit)
