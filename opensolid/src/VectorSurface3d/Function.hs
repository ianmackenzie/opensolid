-- Needed for 'Surface1d.Function * Vector3d = Function'
-- and 'Vector3d * Surface1d.Function = Function' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Surface1d.Function or Vector3d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorSurface3d.Function
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

import Composition
import Direction3d (Direction3d)
import Expression (Expression)
import Expression qualified
import OpenSolid
import Surface1d qualified
import Surface1d.Function qualified
import SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import Units qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified
import VectorCurve3d (VectorCurve3d)
import VectorCurve3d qualified

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
    Surface1d.Function units ->
    Surface1d.Function units ->
    Surface1d.Function units ->
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
    Surface1d.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product3d1d' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface1d.Function units2 ->
    Function (space @ (units1 :/: units2))
  CrossProduct' ::
    Function (space @ units1) ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

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
  Multiplication (Surface1d.Function units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Surface1d.Function units1) (Function (space @ units2)) where
  type
    Surface1d.Function units1 .*. Function (space @ units2) =
      Function (space @ (units1 :*: units2))
  Surface1d.Function.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d3d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Qty units1) (Function (space @ units2)) where
  type Qty units1 .*. Function (space @ units2) = Function (space @ (units1 :*: units2))
  f1 .*. f2 = Surface1d.Function.constant f1 .*. f2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Surface1d.Function units1) (Vector3d (space @ units2)) (Function (space @ units3))

instance Multiplication' (Surface1d.Function units1) (Vector3d (space @ units2)) where
  type
    Surface1d.Function units1 .*. Vector3d (space @ units2) =
      Function (space @ (units1 :*: units2))
  function .*. vector = function .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Surface1d.Function units2) where
  type
    Function (space @ units1) .*. Surface1d.Function units2 =
      Function (space @ (units1 :*: units2))
  Parametric lhs .*. Surface1d.Function.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product3d1d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Qty units2) where
  type Function (space @ units1) .*. Qty units2 = Function (space @ (units1 :*: units2))
  function .*. value = function .*. Surface1d.Function.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Surface1d.Function units2) where
  type
    Vector3d (space @ units1) .*. Surface1d.Function units2 =
      Function (space @ (units1 :*: units2))
  vector .*. function = constant vector .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Surface1d.Function units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Surface1d.Function units2) where
  type
    Function (space @ units1) ./. Surface1d.Function units2 =
      Function (space @ (units1 :/: units2))
  Parametric lhs ./. Surface1d.Function.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Qty units2) where
  type
    Function (space @ units1) ./. Qty units2 =
      Function (space @ (units1 :/: units2))
  function ./. value = function ./. Surface1d.Function.constant value

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units1)) (Function (space_ @ units2))
  where
  type
    Function (space @ units1) .><. Function (space_ @ units2) =
      Function (space @ (units1 :*: units2))
  Parametric lhs .><. Parametric rhs = Parametric (lhs .><. rhs)
  lhs .><. rhs = CrossProduct' lhs rhs

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Function (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Function (space @ units1)) (Vector3d (space_ @ units2))
  where
  type
    Function (space @ units1) .><. Vector3d (space_ @ units2) =
      Function (space @ (units1 :*: units2))
  f .><. v = f .><. constant v

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossMultiplication
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Function (space @ units3))

instance
  space ~ space_ =>
  CrossMultiplication' (Vector3d (space @ units1)) (Function (space_ @ units2))
  where
  type
    Vector3d (space @ units1) .><. Function (space_ @ units2) =
      Function (space @ (units1 :*: units2))
  v .><. f = constant v .><. f

data DotProduct' space units1 units2
  = DotProduct' (Function (space @ units1)) (Function (space @ units2))

deriving instance Show (DotProduct' space units1 units2)

instance Surface1d.Function.Interface (DotProduct' space units1 units2) (units1 :*: units2) where
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
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units1)) (Function (space_ @ units2))
  where
  type
    Function (space @ units1) .<>. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  Parametric lhs .<>. Parametric rhs = Surface1d.Function.Parametric (lhs .<>. rhs)
  lhs .<>. rhs = Surface1d.Function.new (DotProduct' lhs rhs)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Function (space @ units1))
    (Vector3d (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units1)) (Vector3d (space_ @ units2))
  where
  type
    Function (space @ units1) .<>. Vector3d (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotMultiplication
    (Vector3d (space @ units1))
    (Function (space_ @ units2))
    (Surface1d.Function units3)

instance
  space ~ space_ =>
  DotMultiplication' (Vector3d (space @ units1)) (Function (space_ @ units2))
  where
  type
    Vector3d (space @ units1) .<>. Function (space_ @ units2) =
      Surface1d.Function (units1 :*: units2)
  vector .<>. function = constant vector .<>. function

instance
  space ~ space_ =>
  DotMultiplication (Function (space @ units)) (Direction3d space_) (Surface1d.Function units)

instance
  space ~ space_ =>
  DotMultiplication' (Function (space @ units)) (Direction3d space_)
  where
  type Function (space @ units) .<>. Direction3d space_ = Surface1d.Function (units :*: Unitless)
  function .<>. direction = function .<>. Vector3d.unit direction

instance
  space ~ space_ =>
  DotMultiplication (Direction3d space) (Function (space_ @ units)) (Surface1d.Function units)

instance
  space ~ space_ =>
  DotMultiplication' (Direction3d space) (Function (space_ @ units))
  where
  type Direction3d space .<>. Function (space_ @ units) = Surface1d.Function (Unitless :*: units)
  direction .<>. function = Vector3d.unit direction .<>. function

instance
  Composition
    (Surface1d.Function Unitless)
    (VectorCurve3d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (curve :.: function)

instance Interface (VectorCurve3d (space @ units) :.: Surface1d.Function Unitless) (space @ units) where
  evaluateImpl (curve :.: function) uvPoint =
    VectorCurve3d.evaluate curve (Surface1d.Function.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    VectorCurve3d.evaluateBounds curve (Surface1d.Function.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (VectorCurve3d.derivative curve . function) * Surface1d.Function.derivative parameter function

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

xyz ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xyz = XYZ

evaluate :: Function (space @ units) -> UvPoint -> Vector3d (space @ units)
evaluate function uvPoint = case function of
  Function f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XYZ x y z ->
    Vector3d.xyz
      (Surface1d.Function.evaluate x uvPoint)
      (Surface1d.Function.evaluate y uvPoint)
      (Surface1d.Function.evaluate z uvPoint)
  Negated f -> negate (evaluate f uvPoint)
  Sum f1 f2 -> evaluate f1 uvPoint + evaluate f2 uvPoint
  Difference f1 f2 -> evaluate f1 uvPoint - evaluate f2 uvPoint
  Product1d3d' f1 f2 -> Surface1d.Function.evaluate f1 uvPoint .*. evaluate f2 uvPoint
  Product3d1d' f1 f2 -> evaluate f1 uvPoint .*. Surface1d.Function.evaluate f2 uvPoint
  Quotient' f1 f2 -> evaluate f1 uvPoint ./. Surface1d.Function.evaluate f2 uvPoint
  CrossProduct' f1 f2 -> evaluate f1 uvPoint .><. evaluate f2 uvPoint

evaluateBounds :: Function (space @ units) -> UvBounds -> VectorBounds3d (space @ units)
evaluateBounds function uvBounds = case function of
  Function f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XYZ x y z ->
    VectorBounds3d.xyz
      (Surface1d.Function.evaluateBounds x uvBounds)
      (Surface1d.Function.evaluateBounds y uvBounds)
      (Surface1d.Function.evaluateBounds z uvBounds)
  Negated f -> negate (evaluateBounds f uvBounds)
  Sum f1 f2 -> evaluateBounds f1 uvBounds + evaluateBounds f2 uvBounds
  Difference f1 f2 -> evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds
  Product1d3d' f1 f2 -> Surface1d.Function.evaluateBounds f1 uvBounds .*. evaluateBounds f2 uvBounds
  Product3d1d' f1 f2 -> evaluateBounds f1 uvBounds .*. Surface1d.Function.evaluateBounds f2 uvBounds
  Quotient' f1 f2 -> evaluateBounds f1 uvBounds ./. Surface1d.Function.evaluateBounds f2 uvBounds
  CrossProduct' f1 f2 -> evaluateBounds f1 uvBounds .><. evaluateBounds f2 uvBounds

derivative :: SurfaceParameter -> Function (space @ units) -> Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Coerce (derivative parameter f)
  Parametric expression -> Parametric (Expression.surfaceDerivative parameter expression)
  XYZ x y z ->
    XYZ
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
      (Surface1d.Function.derivative parameter z)
  Negated f -> -(derivative parameter f)
  Sum f1 f2 -> derivative parameter f1 + derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - derivative parameter f2
  Product1d3d' f1 f2 ->
    Surface1d.Function.derivative parameter f1 .*. f2 + f1 .*. derivative parameter f2
  Product3d1d' f1 f2 ->
    derivative parameter f1 .*. f2 + f1 .*. Surface1d.Function.derivative parameter f2
  Quotient' f1 f2 ->
    (derivative parameter f1 .*. f2 - f1 .*. Surface1d.Function.derivative parameter f2)
      .!/.! Surface1d.Function.squared' f2
  CrossProduct' f1 f2 -> derivative parameter f1 .><. f2 + f1 .><. derivative parameter f2
