module OpenSolid.Curve2D.Nonzero
  ( derivative
  , offsetLeftwardBy
  , offsetRightwardBy
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Nonzero qualified as Nonzero
import OpenSolid.Prelude
import OpenSolid.VectorCurve.Nonzero qualified as VectorCurve.Nonzero
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

derivative :: Nonzero (Curve2D units) -> Nonzero (VectorCurve2D units)
derivative = Curve.Nonzero.derivative

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Nonzero (Curve2D units) ->
  Result (Curve2D.IsDegenerate units) (Curve2D units)
offsetLeftwardBy offset curve = do
  let tangentCurve = VectorCurve.Nonzero.normalize (derivative curve)
  let offsetCurve = VectorCurve2D.rotateBy Angle.quarterTurn (offset * Nonzero.unwrap tangentCurve)
  Nonzero.unwrap curve & Curve.displaceBy offsetCurve

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Nonzero (Curve2D units) ->
  Result (Curve2D.IsDegenerate units) (Curve2D units)
offsetRightwardBy distance = offsetLeftwardBy -distance
