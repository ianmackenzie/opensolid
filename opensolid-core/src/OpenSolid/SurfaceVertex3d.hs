module OpenSolid.SurfaceVertex3d
  ( SurfaceVertex3d (SurfaceVertex3d, position, normal)
  , position
  , normal
  , normalVector
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

data SurfaceVertex3d space = SurfaceVertex3d
  { position :: Point3d space
  , normal :: Direction3d space
  }

instance HasField "normalVector" (SurfaceVertex3d space) (Vector3d Unitless space) where
  getField = normalVector

{-# INLINE position #-}
position :: SurfaceVertex3d space -> Point3d space
position = (.position)

{-# INLINE normal #-}
normal :: SurfaceVertex3d space -> Direction3d space
normal = (.normal)

normalVector :: SurfaceVertex3d space -> Vector3d Unitless space
normalVector vertex = Vector3d.unit (normal vertex)
