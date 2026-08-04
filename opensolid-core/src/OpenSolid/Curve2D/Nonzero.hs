module OpenSolid.Curve2D.Nonzero
  ( point
  , derivative
  , tangentDirection
  , curvatureVector
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

point :: Nonzero (Curve2D units) -> Number -> Point2D units
point = Curve.Nonzero.point

derivative :: Nonzero (Curve2D units) -> Nonzero (VectorCurve2D units)
derivative = Curve.Nonzero.derivative

tangentDirection :: Nonzero (Curve2D units) -> Number -> Direction2D
tangentDirection = Curve.Nonzero.tangentDirection

curvatureVector ::
  Units.Inverse units inverseUnits =>
  Nonzero (Curve2D units) ->
  Number ->
  Vector2D inverseUnits
curvatureVector = Curve.Nonzero.curvatureVector

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
