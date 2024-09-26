module Surface1d.Function.Expression
  ( Expression (Expression, Constant, Parameter)
  , derivative
  , evaluate
  , bounds
  , toAst
  , squared'
  , sqrt'
  , sin
  , cos
  )
where

import Angle qualified
import Bounds2d qualified
import Float qualified
import Jit qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import {-# SOURCE #-} Surface1d.Function qualified as Function
import Typeable qualified
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified

data Expression units where
  Expression ::
    Function.Interface function units =>
    function ->
    Expression units
  Constant ::
    Qty units ->
    Expression units
  Parameter ::
    Parameter ->
    Expression Unitless
  Negated ::
    Expression units ->
    Expression units
  Sum ::
    Expression units ->
    Expression units ->
    Expression units
  Difference ::
    Expression units ->
    Expression units ->
    Expression units
  Product' ::
    (Known units1, Known units2) =>
    Expression units1 ->
    Expression units2 ->
    Expression (units1 :*: units2)
  Quotient' ::
    (Known units1, Known units2) =>
    Expression units1 ->
    Expression units2 ->
    Expression (units1 :/: units2)
  Squared' ::
    Known units =>
    Expression units ->
    Expression (units :*: units)
  SquareRoot' ::
    Expression (units :*: units) ->
    Expression units
  Sin ::
    Expression Radians ->
    Expression Unitless
  Cos ::
    Expression Radians ->
    Expression Unitless
  Coerce ::
    (Known units1, Known units2) =>
    Expression units1 ->
    Expression units2

deriving instance Show (Expression units)

instance Eq (Expression units) where
  expression1 == expression2 = case expression1 of
    Expression f1 | Expression f2 <- expression2 -> Typeable.equal f1 f2 | otherwise -> False
    Constant x | Constant y <- expression2 -> x == y | otherwise -> False
    Parameter p1 | Parameter p2 <- expression2 -> p1 == p2 | otherwise -> False
    Negated f1 | Negated f2 <- expression2 -> f1 == f2 | otherwise -> False
    Sum lhs1 rhs1 | Sum lhs2 rhs2 <- expression2 -> lhs1 == lhs2 && rhs1 == rhs2 | otherwise -> False
    Difference lhs1 rhs1 | Difference lhs2 rhs2 <- expression2 -> lhs1 == lhs2 && rhs1 == rhs2 | otherwise -> False
    Product' lhs1 rhs1 | Product' lhs2 rhs2 <- expression2 -> lhs1 == lhs2 && rhs1 == rhs2 | otherwise -> False
    Quotient' lhs1 rhs1 | Quotient' lhs2 rhs2 <- expression2 -> lhs1 == lhs2 && rhs1 == rhs2 | otherwise -> False
    Squared' f1 | Squared' f2 <- expression2 -> f1 == f2 | otherwise -> False
    SquareRoot' f1 | SquareRoot' f2 <- expression2 -> f1 == f2 | otherwise -> False
    Sin f1 | Sin f2 <- expression2 -> f1 == f2 | otherwise -> False
    Cos f1 | Cos f2 <- expression2 -> f1 == f2 | otherwise -> False
    Coerce f1 | Coerce f2 <- expression2 -> Typeable.equal f1 f2 | otherwise -> False

instance HasUnits (Expression units) where
  type UnitsOf (Expression units) = units

instance (Known unitsA, Known unitsB) => Units.Coercion (Expression unitsA) (Expression unitsB) where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce function) = Coerce function
  coerce expression = Coerce expression

instance Known units => Negation (Expression units) where
  negate (Constant x) = Constant (negate x)
  negate (Coerce function) = Coerce (negate function)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate (Quotient' f1 f2) = negate f1 ./. f2
  negate function = Negated function

instance Known units => Multiplication Sign (Expression units) (Expression units)

instance Known units => Multiplication (Expression units) Sign (Expression units)

instance Known units => Multiplication' Sign (Expression units) where
  type Sign .*. Expression units = Expression (Unitless :*: units)
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Known units => Multiplication' (Expression units) Sign where
  type Expression units .*. Sign = Expression (units :*: Unitless)
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance units ~ units_ => Addition (Expression units) (Expression units_) (Expression units) where
  Constant x + function | x == Qty.zero = function
  function + Constant x | x == Qty.zero = function
  Constant x + Constant y = Constant (x + y)
  function1 + function2 = Sum function1 function2

instance units ~ units_ => Addition (Expression units) (Qty units_) (Expression units) where
  expression + value = expression + Constant value

instance units ~ units_ => Addition (Qty units) (Expression units_) (Expression units) where
  value + expression = Constant value + expression

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Expression units1) (Expression units2) (Expression units1)
  where
  Constant x - function | x == Qty.zero = negate function
  function - Constant x | x == Qty.zero = function
  Constant x - Constant y = Constant (x - y)
  function1 - function2 = Difference function1 function2

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Expression units1) (Qty units2) (Expression units1)
  where
  expression - value = expression - Constant value

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Qty units1) (Expression units2) (Expression units1)
  where
  value - expression = Constant value - expression

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Expression units1) (Expression units2) (Expression units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Expression units1) (Expression units2)
  where
  type Expression units1 .*. Expression units2 = Expression (units1 :*: units2)
  Constant x .*. _ | x == Qty.zero = Constant Qty.zero
  _ .*. Constant x | x == Qty.zero = Constant Qty.zero
  Constant x .*. Constant y = Constant (x .*. y)
  Constant x .*. function | x == Units.coerce 1.0 = Units.coerce function
  Constant x .*. function | x == Units.coerce -1.0 = Units.coerce (negate function)
  Constant x .*. Negated c = negate x .*. c
  f1 .*. (Constant x) = Units.commute (Constant x .*. f1)
  Constant x .*. Product' (Constant y) c = Units.rightAssociate ((x .*. y) .*. c)
  function1 .*. function2 = Product' function1 function2

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Expression units1) (Qty units2) (Expression units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Expression units1) (Qty units2)
  where
  type Expression units1 .*. Qty units2 = Expression (units1 :*: units2)
  expression .*. value = expression .*. Constant value

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Qty units1) (Expression units2) (Expression units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Qty units1) (Expression units2)
  where
  type Qty units1 .*. Expression units2 = Expression (units1 :*: units2)
  value .*. function = Constant value .*. function

instance Known units => Multiplication (Expression units) Int (Expression units)

instance Known units => Multiplication' (Expression units) Int where
  type Expression units .*. Int = Expression (units :*: Unitless)
  expression .*. value = expression .*. Float.int value

instance Known units => Multiplication Int (Expression units) (Expression units)

instance Known units => Multiplication' Int (Expression units) where
  type Int .*. Expression units = Expression (Unitless :*: units)
  value .*. expression = Float.int value .*. expression

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Expression units1) (Expression units2) (Expression units3)

instance (Known units1, Known units2) => Division' (Expression units1) (Expression units2) where
  type Expression units1 ./. Expression units2 = Expression (units1 :/: units2)
  Constant x ./. _ | x == Qty.zero = Constant Qty.zero
  Constant x ./. Constant y = Constant (x ./. y)
  function ./. Constant x = (1 ./. x) .*^ function
  function1 ./. function2 = Quotient' function1 function2

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Expression units1) (Qty units2) (Expression units3)

instance
  (Known units1, Known units2) =>
  Division' (Expression units1) (Qty units2)
  where
  type Expression units1 ./. Qty units2 = Expression (units1 :/: units2)
  function ./. value = function ./. Constant value

instance Known units => Division (Expression units) Int (Expression units)

instance Known units => Division' (Expression units) Int where
  type Expression units ./. Int = Expression (units :/: Unitless)
  expression ./. value = expression ./. Float.int value

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Qty units1) (Expression units2) (Expression units3)

instance
  (Known units1, Known units2) =>
  Division' (Qty units1) (Expression units2)
  where
  type Qty units1 ./. Expression units2 = Expression (units1 :/: units2)
  value ./. expression = Constant value ./. expression

instance
  (Known units1, Known units2, Units.Inverse units1 units2) =>
  Division Int (Expression units1) (Expression units2)

instance Known units => Division' Int (Expression units) where
  type Int ./. Expression units = Expression (Unitless :/: units)
  value ./. expression = Float.int value ./. expression

evaluate :: Expression units -> Uv.Point -> Qty units
evaluate function uv = case function of
  Expression f -> Function.evaluateImpl f uv
  Constant x -> x
  Coerce f -> Units.coerce (evaluate f uv)
  Parameter U -> Point2d.xCoordinate uv
  Parameter V -> Point2d.yCoordinate uv
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product' f1 f2 -> evaluate f1 uv .*. evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. evaluate f2 uv
  Squared' f -> Qty.squared' (evaluate f uv)
  SquareRoot' f -> Qty.sqrt' (evaluate f uv)
  Sin f -> Angle.sin (evaluate f uv)
  Cos f -> Angle.cos (evaluate f uv)

bounds :: Expression units -> Uv.Bounds -> Range units
bounds expression uv = case expression of
  Expression f -> Function.boundsImpl f uv
  Constant x -> Range.constant x
  Coerce f -> Units.coerce (bounds f uv)
  Parameter U -> Bounds2d.xCoordinate uv
  Parameter V -> Bounds2d.yCoordinate uv
  Negated f -> negate (bounds f uv)
  Sum f1 f2 -> bounds f1 uv + bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - bounds f2 uv
  Product' f1 f2 -> bounds f1 uv .*. bounds f2 uv
  Quotient' f1 f2 -> bounds f1 uv ./. bounds f2 uv
  Squared' f -> Range.squared' (bounds f uv)
  SquareRoot' f -> Range.sqrt' (bounds f uv)
  Sin f -> Range.sin (bounds f uv)
  Cos f -> Range.cos (bounds f uv)

derivative :: Known units => Parameter -> Expression units -> Expression units
derivative varyingParameter expression =
  case expression of
    Expression f -> Function.unwrap (Function.derivativeImpl varyingParameter f)
    Constant _ -> Constant Qty.zero
    Coerce f -> Units.coerce (derivative varyingParameter f)
    Parameter p -> if p == varyingParameter then Constant 1.0 else Constant 0.0
    Negated f -> negate (derivative varyingParameter f)
    Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
    Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
    Product' f1 f2 -> derivative varyingParameter f1 .*. f2 + f1 .*. derivative varyingParameter f2
    Quotient' f1 f2 ->
      (derivative varyingParameter f1 .*. f2 - f1 .*. derivative varyingParameter f2)
        .!/.! squared' f2
    Squared' f -> 2 * f .*. derivative varyingParameter f
    SquareRoot' f -> derivative varyingParameter f .!/! (2 * sqrt' f)
    Sin f -> cos f * (derivative varyingParameter f / Angle.radian)
    Cos f -> negate (sin f) * (derivative varyingParameter f / Angle.radian)

squared' :: Known units => Expression units -> Expression (units :*: units)
squared' (Constant x) = Constant (x .*. x)
squared' (Negated f) = squared' f
squared' (Cos f) = Units.unspecialize (cosSquared f)
squared' (Sin f) = Units.unspecialize (sinSquared f)
squared' expression = Squared' expression

sqrt' :: Expression (units :*: units) -> Expression units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' expression = SquareRoot' expression

sin :: Expression Radians -> Expression Unitless
sin (Constant x) = Constant (Angle.sin x)
sin function = Sin function

cos :: Expression Radians -> Expression Unitless
cos (Constant x) = Constant (Angle.cos x)
cos function = Cos function

cosSquared :: Expression Radians -> Expression Unitless
cosSquared f = 0.5 * cos (2 * f) + 0.5

sinSquared :: Expression Radians -> Expression Unitless
sinSquared f = 0.5 - 0.5 * cos (2 * f)

toAst :: Expression units -> Jit.Ast Uv.Point Float
toAst function = case function of
  Expression f -> Function.toAstImpl f
  Constant x -> Jit.constant (Units.coerce x)
  Parameter U -> Jit.xCoordinate Jit.input
  Parameter V -> Jit.yCoordinate Jit.input
  Negated f -> Jit.negate (toAst f)
  Sum f1 f2 -> Jit.sum (toAst f1) (toAst f2)
  Difference f1 f2 -> Jit.difference (toAst f1) (toAst f2)
  Product' f1 f2 -> Jit.product (toAst f1) (toAst f2)
  Quotient' f1 f2 -> Jit.quotient (toAst f1) (toAst f2)
  Squared' f -> Jit.squared (toAst f)
  SquareRoot' f -> Jit.sqrt (toAst f)
  Sin f -> Jit.sin (toAst f)
  Cos f -> Jit.cos (toAst f)
  Coerce f -> toAst f
