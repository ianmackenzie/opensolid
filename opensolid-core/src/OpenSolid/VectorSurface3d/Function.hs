module OpenSolid.VectorSurface3d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , zero
  , constant
  , xyz
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Composition
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude
import OpenSolid.Surface qualified as Surface
import OpenSolid.Surface.Function qualified as Surface.Function
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Vector3d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Parametric ::
    Expression UvPoint (Vector3d (space @ units)) ->
    Function (space @ units)
  XYZ ::
    Surface.Function units ->
    Surface.Function units ->
    Surface.Function units ->
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
  Product1d3d' ::
    Surface.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product3d1d' ::
    Function (space @ units1) ->
    Surface.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface.Function units2 ->
    Function (space @ (units1 :/: units2))
  CrossProduct' ::
    Function (space @ units1) ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce function = case function of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce f -> Coerce f
    _ -> Coerce function

instance Negation (Function (space @ units)) where
  negate function = case function of
    Coerce f -> Coerce -f
    Parametric expression -> Parametric -expression
    XYZ x y z -> XYZ -x -y -z
    Negated f -> f
    Difference f1 f2 -> Difference f2 f1
    Product1d3d' f1 f2 -> Product1d3d' -f1 f2
    Product3d1d' f1 f2 -> Product3d1d' f1 -f2
    _ -> Negated function

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
  lhs + Negated rhs = lhs - rhs
  Negated lhs + rhs = rhs - lhs
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
  f + v = f + constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  v + f = constant v + f

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
  lhs - Negated rhs = lhs + rhs
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
  f - v = f - constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  v - f = constant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Surface.Function units1) (Function (space @ units2)) (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Surface.Function units1)
    (Function (space @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  Surface.Function.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d3d' lhs rhs

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
  f1 .*. f2 = Surface.Function.constant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Surface.Function units2) (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Function (space @ units1))
    (Surface.Function units2)
    (Function (space @ (units1 :*: units2)))
  where
  Parametric lhs .*. Surface.Function.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product3d1d' lhs rhs

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
  function .*. value = function .*. Surface.Function.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Surface.Function units2) (Function (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (Function (space @ units1))
    (Surface.Function units2)
    (Function (space @ (units1 :/: units2)))
  where
  Parametric lhs ./. Surface.Function.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

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
  function ./. value = function ./. Surface.Function.constant value

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ units3))
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  Parametric lhs .><. Parametric rhs = Parametric (lhs .><. rhs)
  lhs .><. rhs = CrossProduct' lhs rhs

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Function (space @ units3))
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  f .><. v = f .><. constant v

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ units3))
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space ~ space_ =>
  CrossMultiplication'
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  v .><. f = constant v .><. f

data DotProduct' space units1 units2
  = DotProduct' (Function (space @ units1)) (Function (space @ units2))

deriving instance Show (DotProduct' space units1 units2)

instance Surface.Function.Interface (DotProduct' space units1 units2) (units1 :*: units2) where
  evaluateImpl (DotProduct' f1 f2) tValue =
    evaluate f1 tValue .<>. evaluate f2 tValue

  evaluateBoundsImpl (DotProduct' f1 f2) tRange =
    evaluateBounds f1 tRange .<>. evaluateBounds f2 tRange

  derivativeImpl parameter (DotProduct' f1 f2) =
    derivative parameter f1 .<>. f2 + f1 .<>. derivative parameter f2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Surface.Function units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Surface.Function (units1 :*: units2))
  where
  Parametric lhs .<>. Parametric rhs = Surface.Function.Parametric (lhs .<>. rhs)
  lhs .<>. rhs = Surface.Function.new (DotProduct' lhs rhs)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Surface.Function units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Surface.Function (units1 :*: units2))
  where
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Surface.Function units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Surface.Function (units1 :*: units2))
  where
  vector .<>. function = constant vector .<>. function

instance
  space ~ space_ =>
  DotMultiplication (Function (space @ units)) (Direction3d space_) (Surface.Function units)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Function (space @ units))
    (Direction3d space_)
    (Surface.Function (units :*: Unitless))
  where
  function .<>. direction = function .<>. Vector3d.unit direction

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (Function (space_ @ units)) (Surface.Function units)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space ~ space_ =>
  DotMultiplication'
    (Direction3d space)
    (Function (space_ @ units))
    (Surface.Function (Unitless :*: units))
  where
  direction .<>. function = Vector3d.unit direction .<>. function

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

xyz ::
  Surface.Function units ->
  Surface.Function units ->
  Surface.Function units ->
  Function (space @ units)
xyz = XYZ

evaluate :: Function (space @ units) -> UvPoint -> Vector3d (space @ units)
evaluate function uvPoint = case function of
  Function f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XYZ x y z ->
    Vector3d.xyz
      (Surface.Function.evaluate x uvPoint)
      (Surface.Function.evaluate y uvPoint)
      (Surface.Function.evaluate z uvPoint)
  Negated f -> negate (evaluate f uvPoint)
  Sum f1 f2 -> evaluate f1 uvPoint + evaluate f2 uvPoint
  Difference f1 f2 -> evaluate f1 uvPoint - evaluate f2 uvPoint
  Product1d3d' f1 f2 -> Surface.Function.evaluate f1 uvPoint .*. evaluate f2 uvPoint
  Product3d1d' f1 f2 -> evaluate f1 uvPoint .*. Surface.Function.evaluate f2 uvPoint
  Quotient' f1 f2 -> evaluate f1 uvPoint ./. Surface.Function.evaluate f2 uvPoint
  CrossProduct' f1 f2 -> evaluate f1 uvPoint .><. evaluate f2 uvPoint

evaluateBounds :: Function (space @ units) -> UvBounds -> VectorBounds3d (space @ units)
evaluateBounds function uvBounds = case function of
  Function f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XYZ x y z ->
    VectorBounds3d.xyz
      (Surface.Function.evaluateBounds x uvBounds)
      (Surface.Function.evaluateBounds y uvBounds)
      (Surface.Function.evaluateBounds z uvBounds)
  Negated f -> negate (evaluateBounds f uvBounds)
  Sum f1 f2 -> evaluateBounds f1 uvBounds + evaluateBounds f2 uvBounds
  Difference f1 f2 -> evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds
  Product1d3d' f1 f2 -> Surface.Function.evaluateBounds f1 uvBounds .*. evaluateBounds f2 uvBounds
  Product3d1d' f1 f2 -> evaluateBounds f1 uvBounds .*. Surface.Function.evaluateBounds f2 uvBounds
  Quotient' f1 f2 -> evaluateBounds f1 uvBounds ./. Surface.Function.evaluateBounds f2 uvBounds
  CrossProduct' f1 f2 -> evaluateBounds f1 uvBounds .><. evaluateBounds f2 uvBounds

derivative :: SurfaceParameter -> Function (space @ units) -> Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Coerce (derivative parameter f)
  Parametric expression -> Parametric (Expression.surfaceDerivative parameter expression)
  XYZ x y z ->
    XYZ
      (Surface.Function.derivative parameter x)
      (Surface.Function.derivative parameter y)
      (Surface.Function.derivative parameter z)
  Negated f -> -(derivative parameter f)
  Sum f1 f2 -> derivative parameter f1 + derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - derivative parameter f2
  Product1d3d' f1 f2 ->
    Surface.Function.derivative parameter f1 .*. f2 + f1 .*. derivative parameter f2
  Product3d1d' f1 f2 ->
    derivative parameter f1 .*. f2 + f1 .*. Surface.Function.derivative parameter f2
  Quotient' f1 f2 ->
    (derivative parameter f1 .*. f2 - f1 .*. Surface.Function.derivative parameter f2)
      .!/.! Surface.Function.squared' f2
  CrossProduct' f1 f2 -> derivative parameter f1 .><. f2 + f1 .><. derivative parameter f2
