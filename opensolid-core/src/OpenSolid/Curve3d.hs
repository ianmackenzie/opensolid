module OpenSolid.Curve3d
  ( Curve3d
  , segments
  , constant
  , parametric
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , reverse
  , bounds
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Curve3d.Function (Function)
import OpenSolid.Curve3d.Function qualified as Function
import OpenSolid.Expression (Expression)
import OpenSolid.Piecewise qualified as Piecewise
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

type Curve3d :: CoordinateSystem -> Type
newtype Curve3d coordinateSystem = Curve3d {segments :: Array (Function coordinateSystem)}

constant :: Point3d (space @ units) -> Curve3d (space @ units)
constant = Curve3d . Array.singleton . Function.constant

parametric :: Expression Float (Point3d (space @ units)) -> Curve3d (space @ units)
parametric = Curve3d . Array.singleton . Function.parametric

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint (Curve3d segments) = Function.evaluate (Array.first segments) 0.0

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint (Curve3d segments) = Function.evaluate (Array.last segments) 1.0

evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluate (Curve3d segments) t = Piecewise.interpolate Function.evaluate segments t

evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds (Curve3d segments) t =
  Piecewise.aggregate Bounds3d.aggregate2 Function.evaluateBounds segments t

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse (Curve3d segments) = Curve3d (Array.reverseMap Function.reverse segments)

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds curve = evaluateBounds curve Range.unit
