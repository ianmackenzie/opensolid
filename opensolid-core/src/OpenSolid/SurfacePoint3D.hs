module OpenSolid.SurfacePoint3D
  ( SurfacePoint3D
  , location
  , point
  , derivativeValue
  )
where

import OpenSolid.Internal.SurfacePoint3D (SurfacePoint3D (..))
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.SurfaceLocation (SurfaceLocation)
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Vector3D (Vector3D)

location :: SurfacePoint3D space -> SurfaceLocation
location = (.location)

point :: SurfacePoint3D space -> Point3D space
point = (.point)

derivativeValue :: SurfaceParameter -> SurfacePoint3D space -> Vector3D Meters space
derivativeValue U surfacePoint = surfacePoint.du
derivativeValue V surfacePoint = surfacePoint.dv
