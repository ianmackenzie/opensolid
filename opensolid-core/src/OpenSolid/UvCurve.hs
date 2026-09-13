module OpenSolid.UvCurve
  ( UvCurve
  , startPoint
  , endPoint
  , pointAt
  , pointOn
  , bounds
  , nondegenerate
  , intersections
  )
where

import OpenSolid.Curve (UvCurve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Intersections qualified as Curve.Intersections
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.IsDegenerate (IsDegenerate)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)

startPoint :: UvCurve -> UvPoint
startPoint = Curve2D.startPoint

endPoint :: UvCurve -> UvPoint
endPoint = Curve2D.endPoint

pointAt :: Number -> UvCurve -> UvPoint
pointAt = Curve2D.pointAt

pointOn :: UvCurve -> Number -> UvPoint
pointOn = Curve2D.pointOn

bounds :: UvCurve -> UvBounds
bounds = Curve2D.bounds

nondegenerate :: UvCurve -> Result (IsDegenerate UvPoint) (Nondegenerate UvCurve)
nondegenerate = unitless Curve.nondegenerate

intersections ::
  UvCurve ->
  UvCurve ->
  Result (Curve.Intersections.Error 2 Unitless Void) (Maybe Curve.Intersections)
intersections = unitless Curve2D.intersections
