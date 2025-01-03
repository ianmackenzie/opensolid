module OpenSolid.Curve3d
  ( Curve3d (Parametric)
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
import OpenSolid.Composition
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Surface1d.Function qualified as Surface1d.Function
import OpenSolid.Surface3d.Function qualified as Surface3d.Function
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Point3d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: function -> VectorCurve3d coordinateSystem
  reverseImpl :: function -> function

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve3d ::
    Interface function (space @ units) =>
    function ->
    Curve3d (space @ units)
  Parametric ::
    Expression Float (Point3d (space @ units)) ->
    Curve3d (space @ units)
  Coerce ::
    Curve3d (space @ units1) ->
    Curve3d (space @ units2)

deriving instance Show (Curve3d (space @ units))

instance HasUnits (Curve3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve3d (space1 @ unitsA)) (Curve3d (space2 @ unitsB))
  where
  coerce f = case f of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce function -> Coerce function
    function -> Coerce function

instance
  Composition
    (Surface1d.Function.Function Unitless)
    (Curve3d (space @ units))
    (Surface3d.Function.Function (space @ units))
  where
  Parametric curve . Surface1d.Function.Parametric function =
    Surface3d.Function.Parametric (curve . function)
  curveFunction . surfaceFunction = Surface3d.Function.new (curveFunction :.: surfaceFunction)

instance
  Surface3d.Function.Interface
    (Curve3d (space @ units) :.: Surface1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (curveFunction :.: surfaceFunction) uvPoint =
    evaluate curveFunction $
      Surface1d.Function.evaluate surfaceFunction uvPoint

  evaluateBoundsImpl (curveFunction :.: surfaceFunction) uvBounds =
    evaluateBounds curveFunction $
      Surface1d.Function.evaluateBounds surfaceFunction uvBounds

  derivativeImpl parameter (curveFunction :.: surfaceFunction) =
    (derivative curveFunction . surfaceFunction)
      * Surface1d.Function.derivative parameter surfaceFunction

new :: Interface function (space @ units) => function -> Curve3d (space @ units)
new = Curve3d

constant :: Point3d (space @ units) -> Curve3d (space @ units)
constant = Parametric . Expression.constant

parametric :: Expression Float (Point3d (space @ units)) -> Curve3d (space @ units)
parametric = Parametric

evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluate f tValue = case f of
  Parametric expression -> Expression.evaluate expression tValue
  Curve3d curve -> evaluateImpl curve tValue
  Coerce curve -> Units.coerce (evaluate curve tValue)

evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds f tRange = case f of
  Parametric expression -> Expression.evaluateBounds expression tRange
  Curve3d curve -> evaluateBoundsImpl curve tRange
  Coerce curve -> Units.coerce (evaluateBounds curve tRange)

derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
derivative f = case f of
  Parametric expression -> VectorCurve3d.Parametric (Expression.curveDerivative expression)
  Curve3d curve -> derivativeImpl curve
  Coerce curve -> Units.coerce (derivative curve)

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse f = case f of
  Parametric expression -> Parametric (expression . Expression.r)
  Curve3d curve -> Curve3d (reverseImpl curve)
  Coerce curve -> Units.coerce (reverse curve)
