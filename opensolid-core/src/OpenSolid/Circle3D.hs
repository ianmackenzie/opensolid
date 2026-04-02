module OpenSolid.Circle3D
  ( Circle3D
  , on
  , centerPoint
  , radius
  , diameter
  , normalDirection
  , plane
  )
where

import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude

data Circle3D space = Circle3D
  { plane :: Plane3D space
  , radius :: Length
  }

on :: Plane3D global -> Circle2D Meters local -> Circle3D global
on givenPlane givenCircle = do
  let originPoint = Point3D.on givenPlane (Circle2D.centerPoint givenCircle)
  Circle3D (Plane3D.moveTo originPoint givenPlane) (Circle2D.radius givenCircle)

centerPoint :: Circle3D space -> Point3D space
centerPoint circle = Plane3D.originPoint (plane circle)

radius :: Circle3D space -> Length
radius = (.radius)

diameter :: Circle3D space -> Length
diameter circle = 2.0 * radius circle

normalDirection :: Circle3D space -> Direction3D space
normalDirection circle = Plane3D.normalDirection (plane circle)

plane :: Circle3D space -> Plane3D space
plane = (.plane)
