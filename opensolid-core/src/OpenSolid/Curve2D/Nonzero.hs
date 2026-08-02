module OpenSolid.Curve2D.Nonzero
  ( pointAt
  , pointOn
  , derivative
  , tangentDirectionAt
  , curvatureVectorAt
  , offsetLeftwardBy
  , offsetRightwardBy
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Nonzero qualified as Nonzero
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.VectorCurve.Nonzero qualified as VectorCurve.Nonzero
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

pointAt :: Number -> Nonzero (Curve2D units) -> Point2D units
pointAt = Curve.Nonzero.pointAt

pointOn :: Nonzero (Curve2D units) -> Number -> Point2D units
pointOn = Curve.Nonzero.pointOn

derivative :: Nonzero (Curve2D units) -> Nonzero (VectorCurve2D units)
derivative = Curve.Nonzero.derivative

tangentDirectionAt :: Number -> Nonzero (Curve2D units) -> Direction2D
tangentDirectionAt = Curve.Nonzero.tangentDirectionAt

curvatureVectorAt ::
  Units.Inverse units inverseUnits =>
  Number ->
  Nonzero (Curve2D units) ->
  Vector2D inverseUnits
curvatureVectorAt = Curve.Nonzero.curvatureVectorAt

offsetLeftwardBy ::
  Tolerance units =>
  Quantity units ->
  Nonzero (Curve2D units) ->
  Curve2D units
offsetLeftwardBy offset curve = do
  let tangentCurve = VectorCurve.Nonzero.normalize (derivative curve)
  let offsetCurve = VectorCurve2D.rotateBy Angle.quarterTurn (offset * Nonzero.unwrap tangentCurve)
  Nonzero.unwrap curve + offsetCurve

offsetRightwardBy ::
  Tolerance units =>
  Quantity units ->
  Nonzero (Curve2D units) ->
  Curve2D units
offsetRightwardBy distance = offsetLeftwardBy -distance
