module OpenSolid.Arc3D
  ( Arc3D
  , with
  , plane
  , centerPoint
  , radius
  , startAngle
  , endAngle
  , sweptAngle
  , startPoint
  , endPoint
  , toCircle
  , reverse
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Circle3D (Circle3D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector3D (Vector3D)

type role Arc3D nominal

data Arc3D space = Arc3D
  { plane :: Plane3D space
  , radius :: Length
  , startAngle :: Angle
  , endAngle :: Angle
  }

with ::
  "plane" ::: Plane3D space ->
  "radius" ::: Length ->
  "startAngle" ::: Angle ->
  "endAngle" ::: Angle ->
  Arc3D space
with
  ("plane" ::: givenPlane)
  ("radius" ::: givenRadius)
  ("startAngle" ::: givenStartAngle)
  ("endAngle" ::: givenEndAngle) =
    Arc3D
      { plane = givenPlane
      , radius = givenRadius
      , startAngle = givenStartAngle
      , endAngle = givenEndAngle
      }

plane :: Arc3D space -> Plane3D space
plane = (.plane)

centerPoint :: Arc3D space -> Point3D space
centerPoint arc = Plane3D.originPoint (plane arc)

radius :: Arc3D space -> Length
radius = (.radius)

startAngle :: Arc3D space -> Angle
startAngle = (.startAngle)

endAngle :: Arc3D space -> Angle
endAngle = (.endAngle)

sweptAngle :: Arc3D space -> Angle
sweptAngle arc = endAngle arc - startAngle arc

instance At (Arc3D space) Number (Point3D space) where
  arc @ t = do
    let angle = Quantity.interpolateFrom (startAngle arc) (endAngle arc) t
    on (plane arc) (Point2D.polar (radius arc) angle)

startPoint :: Arc3D space -> Point3D space
startPoint arc = arc @ 0.0

endPoint :: Arc3D space -> Point3D space
endPoint arc = arc @ 1.0

toCircle :: Arc3D space -> Circle3D space
toCircle arc =
  on (plane arc) do
    Circle2D.with {centerPoint = Point2D.origin, diameter = 2.0 * radius arc}

reverse :: Arc3D space -> Arc3D space
reverse (Arc3D p r a b) = Arc3D p r b a

transformBy ::
  Transform.Tag.IsOrthonormal tag =>
  Transform3D tag space ->
  Arc3D space ->
  Arc3D space
transformBy transform (Arc3D p r a b) = Arc3D (Plane3D.transformBy transform p) r a b

translateBy :: Vector3D Meters space -> Arc3D space -> Arc3D space
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D space -> Length -> Arc3D space -> Arc3D space
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D space -> Length -> Arc3D space -> Arc3D space
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D space -> Angle -> Arc3D space -> Arc3D space
rotateAround = Transform3D.rotateAroundImpl transformBy

mirrorAcross :: Plane3D space -> Arc3D space -> Arc3D space
mirrorAcross = Transform3D.mirrorAcrossImpl transformBy
