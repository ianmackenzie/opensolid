module OpenSolid.Curve3d.Internal
  ( Curve3d (..)
  , Interface (..)
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  , bounds
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Parametric ::
    Expression Float (Point3d (space @ units)) ->
    Curve3d (space @ units)
  Curve ::
    Interface curve (space @ units) =>
    curve ->
    Curve3d (space @ units)
  Coerce ::
    Curve3d (space @ units1) ->
    Curve3d (space @ units2)

deriving instance Show (Curve3d (space @ units))

instance HasUnits (Curve3d (space @ units)) where
  type UnitsOf (Curve3d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Curve3d (space1 @ unitsA)) (Curve3d (space2 @ unitsB))
  where
  coerce curve = case curve of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce c -> Coerce c
    _ -> Coerce curve

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point3d coordinateSystem
  endPointImpl :: curve -> Point3d coordinateSystem
  evaluateImpl :: curve -> Float -> Point3d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds3d coordinateSystem

instance Interface (Curve3d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  boundsImpl = bounds

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint curve = case curve of
  Parametric expression -> Expression.evaluate expression 0.0
  Curve c -> startPointImpl c
  Coerce c -> Units.coerce (startPoint c)

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint curve = case curve of
  Parametric expression -> Expression.evaluate expression 1.0
  Curve c -> endPointImpl c
  Coerce c -> Units.coerce (endPoint c)

evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluate curve tValue = case curve of
  Parametric expression -> Expression.evaluate expression tValue
  Curve c -> evaluateImpl c tValue
  Coerce c -> Units.coerce (evaluate c tValue)

evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds curve tRange = case curve of
  Parametric expression -> Expression.evaluateBounds expression tRange
  Curve c -> evaluateBoundsImpl c tRange
  Coerce c -> Units.coerce (evaluateBounds c tRange)

derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
derivative curve = case curve of
  Parametric expression -> VectorCurve3d.Parametric (Expression.curveDerivative expression)
  Curve c -> derivativeImpl c
  Coerce c -> Units.coerce (derivative c)

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse curve = case curve of
  Parametric expression -> Parametric (expression . Expression.r)
  Curve c -> Curve (reverseImpl c)
  Coerce c -> Units.coerce (reverse c)

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds curve = case curve of
  Parametric expression -> Expression.evaluateBounds expression Range.unit
  Curve c -> boundsImpl c
  Coerce c -> Units.coerce (bounds c)
