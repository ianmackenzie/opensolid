module OpenSolid.Arc3D
  ( Arc3D
  , on
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
import OpenSolid.Arc2D (Arc2D)
import OpenSolid.Arc2D qualified as Arc2D
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Circle3D (Circle3D)
import OpenSolid.Circle3D qualified as Circle3D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector3D (Vector3D)

data Arc3D space = Arc3D
  { plane :: Plane3D space
  , radius :: Length
  , startAngle :: Angle
  , endAngle :: Angle
  }

on :: Plane3D space -> Arc2D Meters -> Arc3D space
on givenPlane givenArc = do
  let originPoint = Point3D.on givenPlane (Arc2D.centerPoint givenArc)
  Arc3D
    (Plane3D.moveTo originPoint givenPlane)
    (Arc2D.radius givenArc)
    (Arc2D.startAngle givenArc)
    (Arc2D.endAngle givenArc)

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

point :: Arc3D space -> Number -> Point3D space
point (Arc3D p r a b) t = Point3D.on p (Point2D.polar r (Quantity.interpolateFrom a b t))

startPoint :: Arc3D space -> Point3D space
startPoint arc = point arc 0.0

endPoint :: Arc3D space -> Point3D space
endPoint arc = point arc 1.0

toCircle :: Arc3D space -> Circle3D space
toCircle arc = Circle3D.on (plane arc) (Circle2D.withRadius (radius arc) Point2D.origin)

reverse :: Arc3D space -> Arc3D space
reverse (Arc3D p r a b) = Arc3D p r b a

transformBy :: Transform.IsOrthonormal tag => Transform3D tag space -> Arc3D space -> Arc3D space
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
