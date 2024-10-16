module Surface1d.Function.Symbolic
  ( Symbolic (Function, Constant, Parameter)
  , derivative
  , evaluate
  , bounds
  , expression
  , squared'
  , sqrt'
  , sin
  , cos
  , valueFunction
  , boundsFunction
  )
where

import Angle qualified
import Bounds2d qualified
import Function (Function)
import Function qualified
import Float qualified
import Maybe qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import {-# SOURCE #-} Surface1d.Function qualified as Function
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified

data Symbolic units where
  Function ::
    Function.Interface function units =>
    function ->
    Symbolic units
  Constant ::
    Qty units ->
    Symbolic units
  Parameter ::
    Parameter ->
    Symbolic Unitless
  Negated ::
    Symbolic units ->
    Symbolic units
  Sum ::
    Symbolic units ->
    Symbolic units ->
    Symbolic units
  Difference ::
    Symbolic units ->
    Symbolic units ->
    Symbolic units
  Product' ::
    Symbolic units1 ->
    Symbolic units2 ->
    Symbolic (units1 :*: units2)
  Quotient' ::
    Symbolic units1 ->
    Symbolic units2 ->
    Symbolic (units1 :/: units2)
  Squared' ::
    Symbolic units ->
    Symbolic (units :*: units)
  SquareRoot' ::
    Symbolic (units :*: units) ->
    Symbolic units
  Sin ::
    Symbolic Radians ->
    Symbolic Unitless
  Cos ::
    Symbolic Radians ->
    Symbolic Unitless
  Coerce ::
    Symbolic units1 ->
    Symbolic units2

deriving instance Show (Symbolic units)

instance HasUnits (Symbolic units) where
  type UnitsOf (Symbolic units) = units

instance Units.Coercion (Symbolic unitsA) (Symbolic unitsB) where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce function) = Coerce function
  coerce symbolic = Coerce symbolic

instance Negation (Symbolic units) where
  negate (Constant x) = Constant (negate x)
  negate (Coerce function) = Coerce (negate function)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate (Quotient' f1 f2) = negate f1 ./. f2
  negate function = Negated function

instance Multiplication Sign (Symbolic units) (Symbolic units)

instance Multiplication (Symbolic units) Sign (Symbolic units)

instance Multiplication' Sign (Symbolic units) where
  type Sign .*. Symbolic units = Symbolic (Unitless :*: units)
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication' (Symbolic units) Sign where
  type Symbolic units .*. Sign = Symbolic (units :*: Unitless)
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance units ~ units_ => Addition (Symbolic units) (Symbolic units_) (Symbolic units) where
  Constant x + function | x == Qty.zero = function
  function + Constant x | x == Qty.zero = function
  Constant x + Constant y = Constant (x + y)
  function1 + function2 = Sum function1 function2

instance units ~ units_ => Addition (Symbolic units) (Qty units_) (Symbolic units) where
  symbolic + value = symbolic + Constant value

instance units ~ units_ => Addition (Qty units) (Symbolic units_) (Symbolic units) where
  value + symbolic = Constant value + symbolic

instance
  units1 ~ units2 =>
  Subtraction (Symbolic units1) (Symbolic units2) (Symbolic units1)
  where
  Constant x - function | x == Qty.zero = negate function
  function - Constant x | x == Qty.zero = function
  Constant x - Constant y = Constant (x - y)
  function1 - function2 = Difference function1 function2

instance
  units1 ~ units2 =>
  Subtraction (Symbolic units1) (Qty units2) (Symbolic units1)
  where
  symbolic - value = symbolic - Constant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (Symbolic units2) (Symbolic units1)
  where
  value - symbolic = Constant value - symbolic

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Symbolic units1) (Symbolic units2) (Symbolic units3)

instance Multiplication' (Symbolic units1) (Symbolic units2) where
  type Symbolic units1 .*. Symbolic units2 = Symbolic (units1 :*: units2)
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
  Units.Product units1 units2 units3 =>
  Multiplication (Symbolic units1) (Qty units2) (Symbolic units3)

instance Multiplication' (Symbolic units1) (Qty units2) where
  type Symbolic units1 .*. Qty units2 = Symbolic (units1 :*: units2)
  symbolic .*. value = symbolic .*. Constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Symbolic units2) (Symbolic units3)

instance Multiplication' (Qty units1) (Symbolic units2) where
  type Qty units1 .*. Symbolic units2 = Symbolic (units1 :*: units2)
  value .*. function = Constant value .*. function

instance Multiplication (Symbolic units) Int (Symbolic units)

instance Multiplication' (Symbolic units) Int where
  type Symbolic units .*. Int = Symbolic (units :*: Unitless)
  symbolic .*. value = symbolic .*. Float.int value

instance Multiplication Int (Symbolic units) (Symbolic units)

instance Multiplication' Int (Symbolic units) where
  type Int .*. Symbolic units = Symbolic (Unitless :*: units)
  value .*. symbolic = Float.int value .*. symbolic

instance
  Units.Quotient units1 units2 units3 =>
  Division (Symbolic units1) (Symbolic units2) (Symbolic units3)

instance Division' (Symbolic units1) (Symbolic units2) where
  type Symbolic units1 ./. Symbolic units2 = Symbolic (units1 :/: units2)
  Constant x ./. _ | x == Qty.zero = Constant Qty.zero
  Constant x ./. Constant y = Constant (x ./. y)
  function ./. Constant x = (1 ./. x) .*^ function
  function1 ./. function2 = Quotient' function1 function2

instance
  Units.Quotient units1 units2 units3 =>
  Division (Symbolic units1) (Qty units2) (Symbolic units3)

instance Division' (Symbolic units1) (Qty units2) where
  type Symbolic units1 ./. Qty units2 = Symbolic (units1 :/: units2)
  function ./. value = function ./. Constant value

instance Division (Symbolic units) Int (Symbolic units)

instance Division' (Symbolic units) Int where
  type Symbolic units ./. Int = Symbolic (units :/: Unitless)
  symbolic ./. value = symbolic ./. Float.int value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Symbolic units2) (Symbolic units3)

instance Division' (Qty units1) (Symbolic units2) where
  type Qty units1 ./. Symbolic units2 = Symbolic (units1 :/: units2)
  value ./. symbolic = Constant value ./. symbolic

instance
  Units.Inverse units1 units2 =>
  Division Int (Symbolic units1) (Symbolic units2)

instance Division' Int (Symbolic units) where
  type Int ./. Symbolic units = Symbolic (Unitless :/: units)
  value ./. symbolic = Float.int value ./. symbolic

evaluate :: Symbolic units -> Uv.Point -> Qty units
evaluate function uv = case function of
  Function f -> Function.evaluateImpl f uv
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

bounds :: Symbolic units -> Uv.Bounds -> Range units
bounds symbolic uv = case symbolic of
  Function f -> Function.boundsImpl f uv
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

derivative :: Parameter -> Symbolic units -> Symbolic units
derivative varyingParameter symbolic =
  case symbolic of
    Function f -> Function.unwrap (Function.derivativeImpl varyingParameter f)
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

squared' :: Symbolic units -> Symbolic (units :*: units)
squared' (Constant x) = Constant (x .*. x)
squared' (Negated f) = squared' f
squared' (Cos f) = Units.unspecialize (cosSquared f)
squared' (Sin f) = Units.unspecialize (sinSquared f)
squared' symbolic = Squared' symbolic

sqrt' :: Symbolic (units :*: units) -> Symbolic units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' symbolic = SquareRoot' symbolic

sin :: Symbolic Radians -> Symbolic Unitless
sin (Constant x) = Constant (Angle.sin x)
sin symbolic = Sin symbolic

cos :: Symbolic Radians -> Symbolic Unitless
cos (Constant x) = Constant (Angle.cos x)
cos symbolic = Cos symbolic

cosSquared :: Symbolic Radians -> Symbolic Unitless
cosSquared f = 0.5 * cos (2 * f) + 0.5

sinSquared :: Symbolic Radians -> Symbolic Unitless
sinSquared f = 0.5 - 0.5 * cos (2 * f)

expression :: Symbolic units -> Maybe (Function Uv.Point (Qty units))
expression symbolic = case symbolic of
  Function f -> Function.expressionImpl f
  Constant x -> Just (Function.constant x)
  Parameter U -> Just Function.u
  Parameter V -> Just Function.v
  Negated f -> Maybe.map negate (expression f)
  Sum f1 f2 -> Maybe.map2 (+) (expression f1) (expression f2)
  Difference f1 f2 -> Maybe.map2 (-) (expression f1) (expression f2)
  Product' f1 f2 -> Maybe.map2 (.*.) (expression f1) (expression f2)
  Quotient' f1 f2 -> Maybe.map2 (./.) (expression f1) (expression f2)
  Squared' f -> Maybe.map Function.squared' (expression f)
  SquareRoot' f -> Maybe.map Function.sqrt' (expression f)
  Sin f -> Maybe.map Function.sin (expression f)
  Cos f -> Maybe.map Function.cos (expression f)
  Coerce f -> Units.coerce (expression f)

valueFunction :: Symbolic units -> (Uv.Point -> Qty units)
valueFunction symbolic = case expression symbolic of
  Just expr -> Function.valueFunction expr
  Nothing -> case symbolic of
    Function f -> Function.evaluateImpl f
    Constant x -> always x
    Coerce f -> Units.coerce . valueFunction f
    Parameter U -> Point2d.xCoordinate
    Parameter V -> Point2d.yCoordinate
    Negated f -> negate . valueFunction f
    Sum f1 f2 -> do
      let valueFunction1 = valueFunction f1
      let valueFunction2 = valueFunction f2
      \uv -> valueFunction1 uv + valueFunction2 uv
    Difference f1 f2 -> do
      let valueFunction1 = valueFunction f1
      let valueFunction2 = valueFunction f2
      \uv -> valueFunction1 uv - valueFunction2 uv
    Product' f1 f2 -> do
      let valueFunction1 = valueFunction f1
      let valueFunction2 = valueFunction f2
      \uv -> valueFunction1 uv .*. valueFunction2 uv
    Quotient' f1 f2 -> do
      let valueFunction1 = valueFunction f1
      let valueFunction2 = valueFunction f2
      \uv -> valueFunction1 uv ./. valueFunction2 uv
    Squared' f -> Qty.squared' . valueFunction f
    SquareRoot' f -> Qty.sqrt' . valueFunction f
    Sin f -> Angle.sin . valueFunction f
    Cos f -> Angle.cos . valueFunction f

boundsFunction :: Symbolic units -> (Uv.Bounds -> Range units)
boundsFunction symbolic = case expression symbolic of
  Just expr -> Function.boundsFunction expr
  Nothing -> case symbolic of
    Function f -> Function.boundsImpl f
    Constant x -> always (Range.constant x)
    Coerce f -> Units.coerce . boundsFunction f
    Parameter U -> Bounds2d.xCoordinate
    Parameter V -> Bounds2d.yCoordinate
    Negated f -> negate . boundsFunction f
    Sum f1 f2 -> do
      let boundsFunction1 = boundsFunction f1
      let boundsFunction2 = boundsFunction f2
      \uv -> boundsFunction1 uv + boundsFunction2 uv
    Difference f1 f2 -> do
      let boundsFunction1 = boundsFunction f1
      let boundsFunction2 = boundsFunction f2
      \uv -> boundsFunction1 uv - boundsFunction2 uv
    Product' f1 f2 -> do
      let boundsFunction1 = boundsFunction f1
      let boundsFunction2 = boundsFunction f2
      \uv -> boundsFunction1 uv .*. boundsFunction2 uv
    Quotient' f1 f2 -> do
      let boundsFunction1 = boundsFunction f1
      let boundsFunction2 = boundsFunction f2
      \uv -> boundsFunction1 uv ./. boundsFunction2 uv
    Squared' f -> Range.squared' . boundsFunction f
    SquareRoot' f -> Range.sqrt' . boundsFunction f
    Sin f -> Range.sin . boundsFunction f
    Cos f -> Range.cos . boundsFunction f
