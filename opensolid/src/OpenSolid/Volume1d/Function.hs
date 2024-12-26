module OpenSolid.Volume1d.Function
  ( Function (Parametric)
  , Interface (..)
  , evaluate
  , evaluateBounds
  , derivative
  , zero
  , constant
  , u
  , v
  , w
  , parameter
  , new
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  )
where

import Angle qualified
import OpenSolid.Prelude
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Qty qualified as Qty
import Range (Range)
import Range qualified
import Units (Radians)
import Units qualified
import VolumeParameter (UvwBounds, UvwPoint, VolumeParameter (U, V, W))

class Show function => Interface function units | function -> units where
  evaluateImpl :: function -> UvwPoint -> Qty units
  evaluateBoundsImpl :: function -> UvwBounds -> Range units
  derivativeImpl :: VolumeParameter -> function -> Function units

data Function units where
  Function ::
    Interface function units =>
    function ->
    Function units
  Parametric ::
    Expression UvwPoint (Qty units) ->
    Function units
  Coerce ::
    Function units1 ->
    Function units2
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

deriving instance Show (Function units)

instance HasUnits (Function units) where
  type UnitsOf (Function units) = units

instance Units.Coercion (Function unitsA) (Function unitsB) where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance Negation (Function units) where
  negate (Parametric expression) = Parametric (negate expression)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate (Quotient' f1 f2) = negate f1 ./. f2
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

instance units ~ units_ => Subtraction (Function units) (Function units_) (Function units) where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance units ~ units_ => Subtraction (Function units) (Qty units_) (Function units) where
  function - value = function - constant value

instance units ~ units_ => Subtraction (Qty units) (Function units_) (Function units) where
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

evaluate :: Function units -> UvwPoint -> Qty units
evaluate function uvwPoint =
  case function of
    Function f -> evaluateImpl f uvwPoint
    Parametric expression -> Expression.evaluate expression uvwPoint
    Coerce f -> Units.coerce (evaluate f uvwPoint)
    Negated f -> negate (evaluate f uvwPoint)
    Sum f1 f2 -> evaluate f1 uvwPoint + evaluate f2 uvwPoint
    Difference f1 f2 -> evaluate f1 uvwPoint - evaluate f2 uvwPoint
    Product' f1 f2 -> evaluate f1 uvwPoint .*. evaluate f2 uvwPoint
    Quotient' f1 f2 -> evaluate f1 uvwPoint ./. evaluate f2 uvwPoint
    Squared' f -> Qty.squared' (evaluate f uvwPoint)
    SquareRoot' f' -> Qty.sqrt' (evaluate f' uvwPoint)
    Sin f -> Angle.sin (evaluate f uvwPoint)
    Cos f -> Angle.cos (evaluate f uvwPoint)

evaluateBounds :: Function units -> UvwBounds -> Range units
evaluateBounds function uvwBounds =
  case function of
    Function f -> evaluateBoundsImpl f uvwBounds
    Parametric expression -> Expression.evaluateBounds expression uvwBounds
    Coerce f -> Units.coerce (evaluateBounds f uvwBounds)
    Negated f -> negate (evaluateBounds f uvwBounds)
    Sum f1 f2 -> evaluateBounds f1 uvwBounds + evaluateBounds f2 uvwBounds
    Difference f1 f2 -> evaluateBounds f1 uvwBounds - evaluateBounds f2 uvwBounds
    Product' f1 f2 -> evaluateBounds f1 uvwBounds .*. evaluateBounds f2 uvwBounds
    Quotient' f1 f2 -> evaluateBounds f1 uvwBounds ./. evaluateBounds f2 uvwBounds
    Squared' f -> Range.squared' (evaluateBounds f uvwBounds)
    SquareRoot' f' -> Range.sqrt' (evaluateBounds f' uvwBounds)
    Sin f -> Range.sin (evaluateBounds f uvwBounds)
    Cos f -> Range.cos (evaluateBounds f uvwBounds)

derivative :: VolumeParameter -> Function units -> Function units
derivative varyingParameter function =
  case function of
    Function f -> derivativeImpl varyingParameter f
    Parametric expression -> Parametric (Expression.volumeDerivative varyingParameter expression)
    Coerce f -> Units.coerce (derivative varyingParameter f)
    Negated f -> negate (derivative varyingParameter f)
    Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
    Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
    Product' f1 f2 -> derivative varyingParameter f1 .*. f2 + f1 .*. derivative varyingParameter f2
    Quotient' f1 f2 ->
      (derivative varyingParameter f1 .*. f2 - f1 .*. derivative varyingParameter f2)
        .!/.! squared' f2
    Squared' f -> 2.0 * f .*. derivative varyingParameter f
    SquareRoot' f' -> derivative varyingParameter f' .!/! (2.0 * function)
    Sin f -> cos f * (derivative varyingParameter f / Angle.radian)
    Cos f -> negate (sin f) * (derivative varyingParameter f / Angle.radian)

zero :: Function units
zero = constant Qty.zero

constant :: Qty units -> Function units
constant = Parametric . Expression.constant

u :: Function Unitless
u = Parametric Expression.u

v :: Function Unitless
v = Parametric Expression.v

w :: Function Unitless
w = Parametric Expression.w

parameter :: VolumeParameter -> Function Unitless
parameter U = u
parameter V = v
parameter W = w

new :: Interface function units => function -> Function units
new = Function

squared :: Units.Squared units1 units2 => Function units1 -> Function units2
squared = Units.specialize . squared'

squared' :: Function units -> Function (units :*: units)
squared' (Parametric expression) = Parametric (Expression.squared' expression)
squared' (Negated f) = squared' f
squared' (Cos f) = Units.unspecialize (cosSquared f)
squared' (Sin f) = Units.unspecialize (sinSquared f)
squared' function = Squared' function

cosSquared :: Function Radians -> Function Unitless
cosSquared f = 0.5 * cos (2.0 * f) + 0.5

sinSquared :: Function Radians -> Function Unitless
sinSquared f = 0.5 - 0.5 * cos (2.0 * f)

sqrt :: Units.Squared units1 units2 => Function units2 -> Function units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Function (units :*: units) -> Function units
sqrt' (Parametric expression) = Parametric (Expression.sqrt' expression)
sqrt' function' = SquareRoot' function'

sin :: Function Radians -> Function Unitless
sin (Parametric expression) = Parametric (Expression.sin expression)
sin function = Sin function

cos :: Function Radians -> Function Unitless
cos (Parametric expression) = Parametric (Expression.cos expression)
cos function = Cos function
