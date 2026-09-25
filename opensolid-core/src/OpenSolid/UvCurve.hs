module OpenSolid.UvCurve
  ( UvCurve
  , IsDegenerate
  , data IsDegenerate
  , new
  , unsafe
  , startPoint
  , endPoint
  , pointAt
  , pointOn
  , bounds
  , lineFrom
  , arcFrom
  , cornerArc
  , intersections
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Curve (UvCurve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.VectorCurve2D (VectorCurve2D)

type IsDegenerate = Curve.IsDegenerate 2 Unitless Void

{-# COMPLETE IsDegenerate #-}

pattern IsDegenerate :: UvPoint -> IsDegenerate
pattern IsDegenerate point = Curve.IsDegenerate point

new :: Curve2D.Compiled Unitless -> VectorCurve2D Unitless -> Result IsDegenerate UvCurve
new = unitless Curve2D.new

unsafe :: Curve2D.Compiled Unitless -> VectorCurve2D Unitless -> UvCurve
unsafe = unitless Curve2D.unsafe

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

-- | Create a line between two points.
lineFrom :: UvPoint -> UvPoint -> Result IsDegenerate UvCurve
lineFrom = unitless Curve2D.lineFrom

{-| Create an arc from the given start point to the given end point, with the given swept angle.

A positive swept angle means the arc turns counterclockwise (turns to the left),
and a negative swept angle means it turns clockwise (turns to the right).
For example, an arc with a swept angle of positive 90 degrees
is quarter circle that turns to the left.
-}
arcFrom :: UvPoint -> UvPoint -> Angle -> Result IsDegenerate UvCurve
arcFrom = unitless Curve2D.arcFrom

-- | Create an arc for rounding off the corner between two straight lines.
cornerArc ::
  "cornerPoint" ::: UvPoint ->
  "incoming" ::: Direction2D ->
  "outgoing" ::: Direction2D ->
  "radius" ::: Number ->
  Result IsDegenerate UvCurve
cornerArc = unitless Curve2D.cornerArc

intersections :: UvCurve -> UvCurve -> Maybe Curve.Intersections
intersections = unitless Curve2D.intersections
