module OpenSolid.Ap242.EdgeCurve
  ( line2D
  , line3D
  , arc2D
  , arc3D
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Ap242 qualified as Ap242
import OpenSolid.Ap242.Curve qualified as Ap242.Curve
import OpenSolid.Arc2D (Arc2D)
import OpenSolid.Arc2D qualified as Arc2D
import OpenSolid.Arc3D (Arc3D)
import OpenSolid.Arc3D qualified as Arc3D
import OpenSolid.Line2D (Line2D)
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.Line3D (Line3D)
import OpenSolid.Line3D qualified as Line3D
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Step qualified as Step

vertex2D :: Point2D Meters -> Step.Entity
vertex2D point = Step.entity "VERTEX_POINT" [Step.text "", Step.referenceTo (Ap242.point2D point)]

vertex3D :: Point3D space -> Step.Entity
vertex3D point = Step.entity "VERTEX_POINT" [Step.text "", Step.referenceTo (Ap242.point3D point)]

edgeCurve :: Step.Entity -> Step.Entity -> Step.Entity -> Bool -> Step.Entity
edgeCurve startVertex endVertex curve sameSense =
  Step.entity "EDGE_CURVE" $
    [ Step.text ""
    , Step.referenceTo startVertex
    , Step.referenceTo endVertex
    , Step.referenceTo curve
    , Step.bool sameSense
    ]

line2D :: Tolerance Meters => Line2D Meters -> Step.Entity
line2D line =
  edgeCurve
    (vertex2D (Line2D.startPoint line))
    (vertex2D (Line2D.endPoint line))
    (Ap242.Curve.line2D line)
    True

line3D :: Tolerance Meters => Line3D space -> Step.Entity
line3D line =
  edgeCurve
    (vertex3D (Line3D.startPoint line))
    (vertex3D (Line3D.endPoint line))
    (Ap242.Curve.line3D line)
    True

arc2D :: Arc2D Meters -> Step.Entity
arc2D arc =
  edgeCurve
    (vertex2D (Arc2D.startPoint arc))
    (vertex2D (Arc2D.endPoint arc))
    (Ap242.Curve.circle2D (Arc2D.toCircle arc))
    (Arc2D.sweptAngle arc >= Angle.zero)

arc3D :: Arc3D space -> Step.Entity
arc3D arc =
  edgeCurve
    (vertex3D (Arc3D.startPoint arc))
    (vertex3D (Arc3D.endPoint arc))
    (Ap242.Curve.circle3D (Arc3D.toCircle arc))
    (Arc3D.sweptAngle arc >= Angle.zero)
