module OpenSolid.SurfaceVertex3D
  ( SurfaceVertex3D (SurfaceVertex3D)
  , position
  , normalDirection
  , normalVector
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

data SurfaceVertex3D space = SurfaceVertex3D
  { position :: Point3D space
  , normalDirection :: Direction3D space
  }

{-# INLINE position #-}
position :: SurfaceVertex3D space -> Point3D space
position = (.position)

{-# INLINE normalDirection #-}
normalDirection :: SurfaceVertex3D space -> Direction3D space
normalDirection = (.normalDirection)

normalVector :: SurfaceVertex3D space -> Vector3D Unitless space
normalVector = Vector3D.unit . normalDirection
