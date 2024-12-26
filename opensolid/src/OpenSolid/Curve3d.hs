module OpenSolid.Curve3d
  ( Curve3d (Parametric)
  , Interface (..)
  , DegenerateCurve (DegenerateCurve)
  , new
  , constant
  , startPoint
  , endPoint
  , evaluate
  , evaluateBounds
  , derivative
  , reverse
  , bounds
  )
where

import Bounds3d (Bounds3d)
import Error qualified
import OpenSolid.Prelude
import OpenSolid.Curve3d.Internal (Curve3d (Parametric), Interface (..))
import OpenSolid.Curve3d.Internal qualified as Internal
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point3d (Point3d)
import OpenSolid.VectorCurve3d (VectorCurve3d)
import Range (Range)

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, Error.Message)

new :: Interface curve (space @ units) => curve -> Curve3d (space @ units)
new = Internal.Curve

constant :: Point3d (space @ units) -> Curve3d (space @ units)
constant = Internal.Parametric . Expression.constant

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint = Internal.startPoint

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint = Internal.endPoint

evaluate :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
evaluate = Internal.evaluate

evaluateBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
evaluateBounds = Internal.evaluateBounds

derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
derivative = Internal.derivative

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse = Internal.reverse

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds = Internal.bounds
