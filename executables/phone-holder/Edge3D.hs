module Edge3D
  ( Edge3D (..)
  , line
  , arc
  , startPoint
  , endPoint
  , reverse
  , transformBy
  , mirrorAcross
  , entity
  )
where

import OpenSolid.Ap242.EdgeCurve qualified as Ap242.EdgeCurve
import OpenSolid.Arc3D (Arc3D)
import OpenSolid.Arc3D qualified as Arc3D
import OpenSolid.Line3D (Line3D)
import OpenSolid.Line3D qualified as Line3D
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Step qualified as Step
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D

data Edge3D space = Line Sign (Line3D space) | Arc Sign (Arc3D space)

line :: Line3D space -> Edge3D space
line = Line Positive

arc :: Arc3D space -> Edge3D space
arc = Arc Positive

startPoint :: Edge3D space -> Point3D space
startPoint (Line Positive l) = Line3D.startPoint l
startPoint (Line Negative l) = Line3D.endPoint l
startPoint (Arc Positive a) = Arc3D.startPoint a
startPoint (Arc Negative a) = Arc3D.endPoint a

endPoint :: Edge3D space -> Point3D space
endPoint (Line Positive l) = Line3D.endPoint l
endPoint (Line Negative l) = Line3D.startPoint l
endPoint (Arc Positive a) = Arc3D.endPoint a
endPoint (Arc Negative a) = Arc3D.startPoint a

reverse :: Edge3D space -> Edge3D space
reverse (Line sign l) = Line -sign l
reverse (Arc sign a) = Arc -sign a

transformBy ::
  Transform.Tag.IsOrthonormal tag =>
  Transform3D tag space ->
  Edge3D space ->
  Edge3D space
transformBy transform (Line sign l) = Line sign (Line3D.transformBy transform l)
transformBy transform (Arc sign a) = Arc sign (Arc3D.transformBy transform a)

mirrorAcross :: Plane3D space -> Edge3D space -> Edge3D space
mirrorAcross = Transform3D.mirrorAcrossImpl transformBy

entity :: Tolerance Meters => Edge3D space -> Step.Entity
entity edge = do
  let (edgeCurve, sign) =
        case edge of
          Line s l -> (Ap242.EdgeCurve.line3D l, s)
          Arc s a -> (Ap242.EdgeCurve.arc3D a, s)
  Step.entity "ORIENTED_EDGE" $
    [ Step.text ""
    , Step.derived
    , Step.derived
    , Step.referenceTo edgeCurve
    , Step.bool (sign == Positive)
    ]
