module OpenSolid.VectorVolume3d.Function
  ( Function
  , Interface (..)
  , evaluate
  , evaluateBounds
  , derivative
  , zero
  , constant
  , new
  , xyz
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.Volume1d qualified as Volume1d
import OpenSolid.Volume1d.Function qualified as Volume1d.Function
import OpenSolid.VolumeParameter (UvwBounds, UvwPoint, VolumeParameter)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvwPoint -> Vector3d coordinateSystem
  evaluateBoundsImpl :: function -> UvwBounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: VolumeParameter -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Parametric ::
    Expression UvwPoint (Vector3d (space @ units)) ->
    Function (space @ units)
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
  type UnitsOf (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance Negation (Function (space @ units)) where
  negate (Parametric expression) = Parametric (negate expression)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product1d3d f1 f2) = negate f1 .*. f2
  negate (Product3d1d f1 f2) = f1 .*. negate f2
  negate function = Negated function

instance Multiplication Sign (Function (space @ units)) (Function (space @ units)) where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication'
    Sign
    (Function (space @ units))
    (Function (space @ (Unitless :*: units)))
  where
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function (space @ units)) Sign (Function (space @ units)) where
  function * Positive = function
  function * Negative = -function

instance
  Multiplication'
    (Function (space @ units))
    Sign
    (Function (space @ (units :*: Unitless)))
  where
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
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

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
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

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
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Volume1d.Function units1)
    (Function (space @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  Volume1d.Function.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d3d lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Volume1d.Function units2) (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Function (space @ units1))
    (Volume1d.Function units2)
    (Function (space @ (units1 :*: units2)))
  where
  Parametric lhs .*. Volume1d.Function.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product3d1d lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Qty units2) (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Function (space @ units1))
    (Qty units2)
    (Function (space @ (units1 :*: units2)))
  where
  function .*. value = function .*. Volume1d.Function.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function (space @ units2)) (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (Function (space @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  value .*. function = Volume1d.Function.constant value .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Volume1d.Function units2) (Function (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (Function (space @ units1))
    (Volume1d.Function units2)
    (Function (space @ (units1 :/: units2)))
  where
  Parametric lhs ./. Volume1d.Function.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Qty units2) (Function (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (Function (space @ units1))
    (Qty units2)
    (Function (space @ (units1 :/: units2)))
  where
  function ./. value = function ./. Volume1d.Function.constant value

evaluate :: Function (space @ units) -> UvwPoint -> Vector3d (space @ units)
evaluate function uvwPoint =
  case function of
    Function f -> evaluateImpl f uvwPoint
    Parametric expression -> Expression.evaluate expression uvwPoint
    Coerce f -> Units.coerce (evaluate f uvwPoint)
    XYZ x y z ->
      Vector3d.xyz
        (Volume1d.Function.evaluate x uvwPoint)
        (Volume1d.Function.evaluate y uvwPoint)
        (Volume1d.Function.evaluate z uvwPoint)
    Negated f -> negate (evaluate f uvwPoint)
    Sum f1 f2 -> evaluate f1 uvwPoint + evaluate f2 uvwPoint
    Difference f1 f2 -> evaluate f1 uvwPoint - evaluate f2 uvwPoint
    Product1d3d f1 f2 -> Volume1d.Function.evaluate f1 uvwPoint .*. evaluate f2 uvwPoint
    Product3d1d f1 f2 -> evaluate f1 uvwPoint .*. Volume1d.Function.evaluate f2 uvwPoint
    Quotient f1 f2 -> evaluate f1 uvwPoint ./. Volume1d.Function.evaluate f2 uvwPoint

evaluateBounds ::
  Function (space @ units) ->
  UvwBounds ->
  VectorBounds3d (space @ units)
evaluateBounds function uvwBounds =
  case function of
    Function f -> evaluateBoundsImpl f uvwBounds
    Parametric expression -> Expression.evaluateBounds expression uvwBounds
    Coerce f -> Units.coerce (evaluateBounds f uvwBounds)
    XYZ x y z ->
      VectorBounds3d.xyz
        (Volume1d.Function.evaluateBounds x uvwBounds)
        (Volume1d.Function.evaluateBounds y uvwBounds)
        (Volume1d.Function.evaluateBounds z uvwBounds)
    Negated f -> negate (evaluateBounds f uvwBounds)
    Sum f1 f2 -> evaluateBounds f1 uvwBounds + evaluateBounds f2 uvwBounds
    Difference f1 f2 -> evaluateBounds f1 uvwBounds - evaluateBounds f2 uvwBounds
    Product1d3d f1 f2 ->
      Volume1d.Function.evaluateBounds f1 uvwBounds .*. evaluateBounds f2 uvwBounds
    Product3d1d f1 f2 ->
      evaluateBounds f1 uvwBounds .*. Volume1d.Function.evaluateBounds f2 uvwBounds
    Quotient f1 f2 ->
      evaluateBounds f1 uvwBounds ./. Volume1d.Function.evaluateBounds f2 uvwBounds

derivative :: VolumeParameter -> Function units -> Function units
derivative varyingParameter function =
  case function of
    Function f -> derivativeImpl varyingParameter f
    Parametric expression -> Parametric (Expression.volumeDerivative varyingParameter expression)
    Coerce f -> Units.coerce (derivative varyingParameter f)
    XYZ x y z ->
      XYZ
        (Volume1d.Function.derivative varyingParameter x)
        (Volume1d.Function.derivative varyingParameter y)
        (Volume1d.Function.derivative varyingParameter z)
    Negated f -> negate (derivative varyingParameter f)
    Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
    Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
    Product1d3d f1 f2 ->
      Volume1d.Function.derivative varyingParameter f1 .*. f2 + f1 .*. derivative varyingParameter f2
    Product3d1d f1 f2 ->
      derivative varyingParameter f1 .*. f2 + f1 .*. Volume1d.Function.derivative varyingParameter f2
    Quotient f1 f2 ->
      (derivative varyingParameter f1 .*. f2 - f1 .*. Volume1d.Function.derivative varyingParameter f2)
        .!/.! Volume1d.Function.squared' f2

zero :: Function (space @ units)
zero = Parametric Expression.zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

xyz ::
  Volume1d.Function units ->
  Volume1d.Function units ->
  Volume1d.Function units ->
  Function (space @ units)
xyz
  (Volume1d.Function.Parametric x)
  (Volume1d.Function.Parametric y)
  (Volume1d.Function.Parametric z) =
    Parametric (Expression.xyz x y z)
xyz x y z = XYZ x y z
