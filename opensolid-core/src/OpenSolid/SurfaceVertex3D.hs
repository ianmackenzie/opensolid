module OpenSolid.SurfaceVertex3D
  ( SurfaceVertex3D (SurfaceVertex3D, position, normal)
  , position
  , normal
  , normalVector
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

data SurfaceVertex3D space = SurfaceVertex3D
  { position :: Point3D space
  , normal :: Direction3D space
  }

instance HasField "normalVector" (SurfaceVertex3D space) (Vector3D Unitless space) where
  getField = normalVector

{-# INLINE position #-}
position :: SurfaceVertex3D space -> Point3D space
position = (.position)

{-# INLINE normal #-}
normal :: SurfaceVertex3D space -> Direction3D space
normal = (.normal)

normalVector :: SurfaceVertex3D space -> Vector3D Unitless space
normalVector vertex = Vector3D.unit (normal vertex)
