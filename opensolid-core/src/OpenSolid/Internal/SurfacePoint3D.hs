module OpenSolid.Internal.SurfacePoint3D
  ( SurfacePoint3D (..)
  )
where

import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.SurfaceLocation (SurfaceLocation)
import OpenSolid.Vector3D (Vector3D)

data SurfacePoint3D space = SurfacePoint3D
  { location :: SurfaceLocation
  , point :: ~(Point3D space)
  , du :: ~(Vector3D Meters space)
  , dv :: ~(Vector3D Meters space)
  }
