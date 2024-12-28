module OpenSolid.Curve2d.Function
  ( Function (Parametric)
  , Interface (..)
  , TransformBy (TransformBy)
  , new
  , constant
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  , signedDistanceAlong
  , xCoordinate
  , yCoordinate
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Arithmetic qualified as Arithmetic
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Composition
import OpenSolid.Curve1d.Function qualified as Curve1d.Function
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Surface2d.Function ()
import OpenSolid.Text qualified as Text
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2d.Function qualified as VectorCurve2d.Function
import Prelude qualified

type role Function nominal

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function coordinateSystem =>
    function ->
    Function coordinateSystem
  Parametric ::
    Expression Float (Point2d (space @ units)) ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  PlaceIn ::
    Frame2d (global @ units) (Defines local) ->
    Function (local @ units) ->
    Function (global @ units)
  Addition ::
    Function (space @ units) ->
    VectorCurve2d.Function.Function (space @ units) ->
    Function (space @ units)
  Subtraction ::
    Function (space @ units) ->
    VectorCurve2d.Function.Function (space @ units) ->
    Function (space @ units)

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

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Function (space1 @ units1)) (Function (space2 @ units2)) units1
  where
  function1 ~= function2 =
    List.allTrue [evaluate function1 t ~= evaluate function2 t | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality (Function (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  function ~= point = List.allTrue [evaluate function t ~= point | t <- Parameter.samples]

data HasDegeneracy = HasDegeneracy deriving (Eq, Show, Error.Message)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Point2d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> Bounds2d coordinateSystem
  derivativeImpl :: function -> VectorCurve2d.Function.Function coordinateSystem
  reverseImpl :: function -> function
  transformByImpl :: Transform2d tag coordinateSystem -> function -> Function coordinateSystem

instance Interface (Function (space @ units)) (space @ units) where
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  transformByImpl = transformBy

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (VectorCurve2d.Function.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs + VectorCurve2d.Function.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Addition lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (VectorCurve2d.Function.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs - VectorCurve2d.Function.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Subtraction lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (VectorCurve2d.Function.Function (space1 @ units1))
  where
  Parametric lhs - Parametric rhs = VectorCurve2d.Function.Parametric (lhs - rhs)
  lhs - rhs = VectorCurve2d.Function.new (Arithmetic.Difference lhs rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  VectorCurve2d.Function.Interface
    (Arithmetic.Difference (Function (space1 @ units1)) (Function (space2 @ units2)))
    (space1 @ units1)
  where
  evaluateImpl (Arithmetic.Difference lhs rhs) tValue =
    evaluate lhs tValue - evaluate rhs tValue

  evaluateBoundsImpl (Arithmetic.Difference lhs rhs) tRange =
    evaluateBounds lhs tRange - evaluateBounds rhs tRange

  derivativeImpl (Arithmetic.Difference lhs rhs) =
    derivative lhs - derivative rhs

  transformByImpl transform (Arithmetic.Difference lhs rhs) =
    VectorCurve2d.Function.new $
      Arithmetic.Difference
        -- Note the slight hack here:
        -- the definition of VectorCurve2d.Function.Interface states that the units of the transform
        -- do *not* have to match the units of the vector function,
        -- because vectors and vector functions ignore translation
        -- (and the units of the transform are just the units of its translation part).
        -- This would in general mean that we couldn't apply the given transform to a Function,
        -- but in this case it's safe since any translation will cancel out
        -- when the two functions are subtracted from each other.
        (transformBy (Units.coerce transform) lhs)
        (transformBy (Units.coerce transform) rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorCurve2d.Function.Function (space1 @ units1))
  where
  function - point = function - constant point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Function (space2 @ units2))
    (VectorCurve2d.Function.Function (space1 @ units1))
  where
  point - function = constant point - function

instance
  Composition
    (Curve1d.Function.Function Unitless)
    (Function (space @ units))
    (Function (space @ units))
  where
  Parametric expression2d . Curve1d.Function.Parametric expression1d =
    Parametric (expression2d . expression1d)
  function2d . function1d = new (function2d :.: function1d)

instance
  Interface
    (Function (space @ units) :.: Curve1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (function2d :.: function1d) tRange =
    evaluate function2d (Curve1d.Function.evaluate function1d tRange)

  evaluateBoundsImpl (function2d :.: function1d) tRange =
    evaluateBounds function2d (Curve1d.Function.evaluateBounds function1d tRange)

  derivativeImpl (function2d :.: function1d) =
    (derivative function2d . function1d) * Curve1d.Function.derivative function1d

  reverseImpl (function2d :.: function1d) =
    function2d :.: Curve1d.Function.reverse function1d

  transformByImpl transform (function2d :.: function1d) =
    new (transformBy transform function2d . function1d)

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point2d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

evaluate :: Function (space @ units) -> Float -> Point2d (space @ units)
evaluate f tValue = case f of
  Function function -> evaluateImpl function tValue
  Parametric expression -> Expression.evaluate expression tValue
  Coerce function -> Units.coerce (evaluate function tValue)
  PlaceIn frame function -> Point2d.placeIn frame (evaluate function tValue)
  Addition function vectorFunction ->
    evaluate function tValue + VectorCurve2d.Function.evaluate vectorFunction tValue
  Subtraction function vectorFunction ->
    evaluate function tValue - VectorCurve2d.Function.evaluate vectorFunction tValue

evaluateBounds :: Function (space @ units) -> Range Unitless -> Bounds2d (space @ units)
evaluateBounds f tRange = case f of
  Function function -> evaluateBoundsImpl function tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Coerce function -> Units.coerce (evaluateBounds function tRange)
  PlaceIn frame function -> Bounds2d.placeIn frame (evaluateBounds function tRange)
  Addition function vectorFunction ->
    evaluateBounds function tRange + VectorCurve2d.Function.evaluateBounds vectorFunction tRange
  Subtraction function vectorFunction ->
    evaluateBounds function tRange - VectorCurve2d.Function.evaluateBounds vectorFunction tRange

derivative :: Function (space @ units) -> VectorCurve2d.Function.Function (space @ units)
derivative f = case f of
  Function function -> derivativeImpl function
  Parametric expression -> VectorCurve2d.Function.Parametric (Expression.curveDerivative expression)
  Coerce function -> Units.coerce (derivative function)
  PlaceIn frame function -> VectorCurve2d.Function.placeIn frame (derivative function)
  Addition function vectorFunction ->
    derivative function + VectorCurve2d.Function.derivative vectorFunction
  Subtraction function vectorFunction ->
    derivative function - VectorCurve2d.Function.derivative vectorFunction

reverse :: Function (space @ units) -> Function (space @ units)
reverse f = case f of
  Function function -> Function (reverseImpl function)
  Parametric expression -> Parametric (expression . Expression.r)
  Coerce function -> Units.coerce (reverse function)
  PlaceIn frame function -> PlaceIn frame (reverse function)
  Addition function vectorFunction ->
    reverse function + VectorCurve2d.Function.reverse vectorFunction
  Subtraction function vectorFunction ->
    reverse function - VectorCurve2d.Function.reverse vectorFunction

signedDistanceAlong ::
  Axis2d (space @ units) ->
  Function (space @ units) ->
  Curve1d.Function.Function units
signedDistanceAlong axis function = (function - Axis2d.originPoint axis) <> Axis2d.direction axis

xCoordinate :: Function (space @ units) -> Curve1d.Function.Function units
xCoordinate = signedDistanceAlong Axis2d.x

yCoordinate :: Function (space @ units) -> Curve1d.Function.Function units
yCoordinate = signedDistanceAlong Axis2d.y

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Function (local @ units) ->
  Function (global @ units)
placeIn globalFrame function = case function of
  PlaceIn frame localFunction -> PlaceIn (Frame2d.placeIn globalFrame frame) localFunction
  _ -> PlaceIn globalFrame function

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Function (global @ units) ->
  Function (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

transformBy ::
  Transform2d tag (space @ units) ->
  Function (space @ units) ->
  Function (space @ units)
transformBy transform f = case f of
  Function function -> Function (transformByImpl transform function)
  Parametric expression -> Parametric (Expression.Curve2d.transformBy transform expression)
  Coerce function -> Units.coerce (transformBy (Units.coerce transform) function)
  PlaceIn frame function ->
    PlaceIn frame (transformBy (Transform2d.relativeTo frame transform) function)
  Addition function vectorFunction ->
    transformBy transform function + VectorCurve2d.Function.transformBy transform vectorFunction
  Subtraction function vectorFunction ->
    transformBy transform function - VectorCurve2d.Function.transformBy transform vectorFunction

data TransformBy function coordinateSystem where
  TransformBy ::
    Interface function (space @ units) =>
    Transform2d tag (space @ units) ->
    function ->
    TransformBy function (space @ units)

deriving instance Show (TransformBy function (space @ units))

instance Interface (TransformBy function (space @ units)) (space @ units) where
  evaluateImpl (TransformBy transform function) tValue =
    Point2d.transformBy transform (evaluateImpl function tValue)

  evaluateBoundsImpl (TransformBy transform function) tRange =
    Bounds2d.transformBy transform (evaluateBoundsImpl function tRange)

  derivativeImpl (TransformBy transform function) =
    VectorCurve2d.Function.transformBy transform (derivativeImpl function)

  reverseImpl (TransformBy transform function) =
    TransformBy transform (reverseImpl function)

  transformByImpl transform (TransformBy existing function) =
    new (TransformBy (Transform2d.toAffine existing >> Transform2d.toAffine transform) function)

data Synthetic coordinateSystem where
  Synthetic ::
    Function (space @ units) ->
    ~(VectorCurve2d.Function.Function (space @ units)) ->
    Synthetic (space @ units)

instance Show (Synthetic (space @ units)) where
  show (Synthetic function _) = Text.unpack ("Synthetic: " + Text.show function)

-- TODO make Synthetic a first-class constructors,
-- so that it can participate properly in binary operations?
instance Interface (Synthetic (space @ units)) (space @ units) where
  evaluateImpl (Synthetic function _) t = evaluate function t

  evaluateBoundsImpl (Synthetic function _) tRange = evaluateBounds function tRange

  reverseImpl (Synthetic function functionDerivative) =
    Synthetic (reverse function) (-(VectorCurve2d.Function.reverse functionDerivative))

  derivativeImpl (Synthetic _ functionDerivative) = functionDerivative

  transformByImpl transform (Synthetic function functionDerivative) =
    new $
      Synthetic
        (transformBy transform function)
        (VectorCurve2d.Function.transformBy transform functionDerivative)

data SyntheticDerivative coordinateSystem where
  SyntheticDerivative ::
    VectorCurve2d.Function.Function (space @ units) ->
    ~(VectorCurve2d.Function.Function (space @ units)) ->
    SyntheticDerivative (space @ units)

instance Show (SyntheticDerivative (space @ units)) where
  show (SyntheticDerivative function _) = Text.unpack ("SyntheticDerivative: " + Text.show function)

instance
  VectorCurve2d.Function.Interface
    (SyntheticDerivative (space @ units))
    (space @ units)
  where
  evaluateImpl (SyntheticDerivative current _) tValue =
    VectorCurve2d.Function.evaluate current tValue

  evaluateBoundsImpl (SyntheticDerivative current _) tRange =
    VectorCurve2d.Function.evaluateBounds current tRange

  derivativeImpl (SyntheticDerivative _ next) = next

  transformByImpl transform (SyntheticDerivative current next) =
    VectorCurve2d.Function.new $
      SyntheticDerivative
        (VectorCurve2d.Function.transformBy transform current)
        (VectorCurve2d.Function.transformBy transform next)
