module OpenSolid.Curve3d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , constant
  , parametric
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve3d.Function qualified as VectorCurve3d.Function

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Point3d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: function -> VectorCurve3d.Function.Function coordinateSystem
  reverseImpl :: function -> function

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Parametric ::
    Expression Float (Point3d (space @ units)) ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce f = case f of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce function -> Coerce function
    function -> Coerce function

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point3d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

parametric :: Expression Float (Point3d (space @ units)) -> Function (space @ units)
parametric = Parametric

evaluate :: Function (space @ units) -> Float -> Point3d (space @ units)
evaluate f tValue = case f of
  Parametric expression -> Expression.evaluate expression tValue
  Function function -> evaluateImpl function tValue
  Coerce function -> Units.coerce (evaluate function tValue)

evaluateBounds :: Function (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds f tRange = case f of
  Parametric expression -> Expression.evaluateBounds expression tRange
  Function function -> evaluateBoundsImpl function tRange
  Coerce function -> Units.coerce (evaluateBounds function tRange)

derivative :: Function (space @ units) -> VectorCurve3d.Function.Function (space @ units)
derivative f = case f of
  Parametric expression -> VectorCurve3d.Function.Parametric (Expression.curveDerivative expression)
  Function function -> derivativeImpl function
  Coerce function -> Units.coerce (derivative function)

reverse :: Function (space @ units) -> Function (space @ units)
reverse f = case f of
  Parametric expression -> Parametric (expression . Expression.r)
  Function function -> Function (reverseImpl function)
  Coerce function -> Units.coerce (reverse function)
