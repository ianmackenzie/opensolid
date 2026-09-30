module OpenSolid.Circle3D
  ( Circle3D
  , with
  , centerPoint
  , radius
  , diameter
  , normalDirection
  , plane
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude

type role Circle3D nominal

data Circle3D space = Circle3D
  { plane :: Plane3D space
  , radius :: Length
  }

with :: ("plane" ::: Plane3D space, "diameter" ::: Length) -> Circle3D space
with params
  | params.diameter < Length.zero = error "Diameter must not be negative"
  | otherwise = Circle3D {plane = params.plane, radius = 0.5 * params.diameter}

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
