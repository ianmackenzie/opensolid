module OpenSolid.VectorSurface2d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , zero
  , constant
  , xy
  , evaluate
  , evaluateBounds
  , derivative
  , transformBy
  )
where

import OpenSolid.Composition
import OpenSolid.CoordinateSystem (Space)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface2d qualified as Expression.VectorSurface2d
import OpenSolid.Prelude
import OpenSolid.Surface.Function qualified as Surface.Function
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Vector2d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> VectorBounds2d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> Function coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Parametric ::
    Expression UvPoint (Vector2d (space @ units)) ->
    Function (space @ units)
  XY ::
    Surface.Function.Function units ->
    Surface.Function.Function units ->
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
  Product1d2d' ::
    Surface.Function.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product2d1d' ::
    Function (space @ units1) ->
    Surface.Function.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface.Function.Function units2 ->
    Function (space @ (units1 :/: units2))
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    Function (space @ units) ->
    Function (space @ units)

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
    XY x y -> XY -x -y
    Negated f -> f
    Difference f1 f2 -> Difference f2 f1
    Product1d2d' f1 f2 -> Product1d2d' -f1 f2
    Product2d1d' f1 f2 -> Product2d1d' f1 -f2
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
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + Negated rhs = lhs - rhs
  Negated lhs + rhs = rhs - lhs
  lhs + rhs = Sum lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f + v = f + constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  v + f = constant v + f

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - Negated rhs = lhs + rhs
  lhs - rhs = Difference lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f - v = f - constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  v - f = constant v - f

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Surface.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Surface.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ (units1 :*: units2)))
  where
  Surface.Function.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d2d' lhs rhs

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
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
  Multiplication
    (Function (space @ units1))
    (Surface.Function.Function units2)
    (Function (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Function (space @ units1))
    (Surface.Function.Function units2)
    (Function (space @ (units1 :*: units2)))
  where
  Parametric lhs .*. Surface.Function.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product2d1d' lhs rhs

instance
  (space1 ~ space2, Units.Product units1 units2 units3) =>
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
  (space1 ~ space2, Units.Quotient units1 units2 units3) =>
  Division
    (Function (space @ units1))
    (Surface.Function.Function units2)
    (Function (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (Function (space @ units1))
    (Surface.Function.Function units2)
    (Function (space @ (units1 :/: units2)))
  where
  Parametric lhs ./. Surface.Function.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  (space1 ~ space2, Units.Quotient units1 units2 units3) =>
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

data CrossProduct' space units1 units2
  = CrossProduct' (Function (space @ units1)) (Function (space @ units2))

deriving instance Show (CrossProduct' space units1 units2)

instance Surface.Function.Interface (CrossProduct' space units1 units2) (units1 :*: units2) where
  evaluateImpl (CrossProduct' f1 f2) tValue =
    evaluate f1 tValue .><. evaluate f2 tValue

  evaluateBoundsImpl (CrossProduct' f1 f2) tRange =
    evaluateBounds f1 tRange .><. evaluateBounds f2 tRange

  derivativeImpl parameter (CrossProduct' f1 f2) =
    derivative parameter f1 .><. f2 + f1 .><. derivative parameter f2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function units3)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function (units1 :*: units2))
  where
  Parametric lhs .><. Parametric rhs = Surface.Function.Parametric (lhs .><. rhs)
  lhs .><. rhs = Surface.Function.new (CrossProduct' lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Surface.Function.Function units3)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Surface.Function.Function (units1 :*: units2))
  where
  function .><. vector = function .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function units3)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function (units1 :*: units2))
  where
  vector .><. function = constant vector .><. function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function (space1 @ units))
    (Direction2d space2)
    (Surface.Function.Function units)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Function (space1 @ units))
    (Direction2d space2)
    (Surface.Function.Function (units :*: Unitless))
  where
  function .><. direction = function .><. Vector2d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Function (space2 @ units))
    (Surface.Function.Function units)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (Function (space2 @ units))
    (Surface.Function.Function (Unitless :*: units))
  where
  direction .><. function = Vector2d.unit direction .<>. function

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
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function (units1 :*: units2))
  where
  Parametric lhs .<>. Parametric rhs = Surface.Function.Parametric (lhs .<>. rhs)
  lhs .<>. rhs = Surface.Function.new (DotProduct' lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Surface.Function.Function units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Surface.Function.Function (units1 :*: units2))
  where
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Surface.Function.Function (units1 :*: units2))
  where
  vector .<>. function = constant vector .<>. function

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function (space1 @ units))
    (Direction2d space2)
    (Surface.Function.Function units)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function (space1 @ units))
    (Direction2d space2)
    (Surface.Function.Function (units :*: Unitless))
  where
  function .<>. direction = function .<>. Vector2d.unit direction

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Function (space2 @ units))
    (Surface.Function.Function units)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (Function (space2 @ units))
    (Surface.Function.Function (Unitless :*: units))
  where
  direction .<>. function = Vector2d.unit direction .<>. function

instance
  Composition
    (Curve2d UvCoordinates)
    (Function (space @ units))
    (VectorCurve2d (space @ units))
  where
  Parametric function . Curve2d.Parametric curve = VectorCurve2d.Parametric (function . curve)
  function . curve = VectorCurve2d.new (function :.: curve)

instance
  VectorCurve2d.Interface
    (Function (space @ units) :.: Curve2d UvCoordinates)
    (space @ units)
  where
  evaluateImpl (function :.: curve) tValue =
    evaluate function (Curve2d.evaluate curve tValue)

  evaluateBoundsImpl (function :.: curve) tRange =
    evaluateBounds function (Curve2d.evaluateBounds curve tRange)

  derivativeImpl (function :.: curve) = do
    let curveDerivative = Curve2d.derivative curve
    let dudt = VectorCurve2d.xComponent curveDerivative
    let dvdt = VectorCurve2d.yComponent curveDerivative
    (derivative U function . curve) * dudt + (derivative V function . curve) * dvdt

  transformByImpl transform (function :.: curve) = transformBy transform function . curve

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector2d.zero

constant :: Vector2d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

xy ::
  Surface.Function.Function units ->
  Surface.Function.Function units ->
  Function (space @ units)
xy (Surface.Function.Parametric x) (Surface.Function.Parametric y) =
  Parametric (Expression.xy x y)
xy x y = XY x y

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  Function (space @ units) ->
  Function (space @ units)
transformBy transform function = do
  let t = Units.erase (Transform2d.toAffine transform)
  case function of
    Function f -> transformByImpl transform f
    Coerce f -> Coerce (transformBy transform f)
    Parametric expression -> Parametric (Expression.VectorSurface2d.transformBy t expression)
    XY _ _ -> Transformed t function
    Negated arg -> negate (transformBy transform arg)
    Sum lhs rhs -> transformBy transform lhs + transformBy transform rhs
    Difference lhs rhs -> transformBy transform lhs - transformBy transform rhs
    Product1d2d' f1 f2 -> Product1d2d' f1 (transformBy transform f2)
    Product2d1d' f1 f2 -> Product2d1d' (transformBy transform f1) f2
    Quotient' f1 f2 -> Quotient' (transformBy transform f1) f2
    Transformed existing c -> Transformed (existing >> t) c

evaluate :: Function (space @ units) -> UvPoint -> Vector2d (space @ units)
evaluate function uvPoint = case function of
  Function f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XY x y ->
    Vector2d.xy
      (Surface.Function.evaluate x uvPoint)
      (Surface.Function.evaluate y uvPoint)
  Negated f -> negate (evaluate f uvPoint)
  Sum f1 f2 -> evaluate f1 uvPoint + evaluate f2 uvPoint
  Difference f1 f2 -> evaluate f1 uvPoint - evaluate f2 uvPoint
  Product1d2d' f1 f2 -> Surface.Function.evaluate f1 uvPoint .*. evaluate f2 uvPoint
  Product2d1d' f1 f2 -> evaluate f1 uvPoint .*. Surface.Function.evaluate f2 uvPoint
  Quotient' f1 f2 -> evaluate f1 uvPoint ./. Surface.Function.evaluate f2 uvPoint
  Transformed transform f -> Vector2d.transformBy transform (evaluate f uvPoint)

evaluateBounds :: Function (space @ units) -> UvBounds -> VectorBounds2d (space @ units)
evaluateBounds function uvBounds = case function of
  Function f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XY x y ->
    VectorBounds2d.xy
      (Surface.Function.evaluateBounds x uvBounds)
      (Surface.Function.evaluateBounds y uvBounds)
  Negated f -> negate (evaluateBounds f uvBounds)
  Sum f1 f2 -> evaluateBounds f1 uvBounds + evaluateBounds f2 uvBounds
  Difference f1 f2 -> evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds
  Product1d2d' f1 f2 -> Surface.Function.evaluateBounds f1 uvBounds .*. evaluateBounds f2 uvBounds
  Product2d1d' f1 f2 -> evaluateBounds f1 uvBounds .*. Surface.Function.evaluateBounds f2 uvBounds
  Quotient' f1 f2 -> evaluateBounds f1 uvBounds ./. Surface.Function.evaluateBounds f2 uvBounds
  Transformed transform f -> VectorBounds2d.transformBy transform (evaluateBounds f uvBounds)

derivative :: SurfaceParameter -> Function (space @ units) -> Function (space @ units)
derivative varyingParameter function = case function of
  Function f -> derivativeImpl varyingParameter f
  Coerce f -> Coerce (derivative varyingParameter f)
  Parametric expression -> Parametric (Expression.surfaceDerivative varyingParameter expression)
  XY x y ->
    XY
      (Surface.Function.derivative varyingParameter x)
      (Surface.Function.derivative varyingParameter y)
  Negated f -> -(derivative varyingParameter f)
  Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
  Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
  Product1d2d' f1 f2 ->
    Surface.Function.derivative varyingParameter f1 .*. f2
      + f1 .*. derivative varyingParameter f2
  Product2d1d' f1 f2 ->
    derivative varyingParameter f1 .*. f2
      + f1 .*. Surface.Function.derivative varyingParameter f2
  Quotient' f1 f2 -> do
    let numerator =
          derivative varyingParameter f1 .*. f2
            - f1 .*. Surface.Function.derivative varyingParameter f2
    let denominator = Surface.Function.squared' f2
    numerator .!/.! denominator
  Transformed transform f -> transformBy transform (derivative varyingParameter f)
