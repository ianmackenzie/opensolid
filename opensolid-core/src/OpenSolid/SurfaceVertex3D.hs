module OpenSolid.SurfaceVertex3D
  ( SurfaceVertex3D (SurfaceVertex3D)
  , position
  , normalDirection
  , normalVector
  )
where

import Data.Coerce qualified
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Space qualified as Space
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

data SurfaceVertex3D space = SurfaceVertex3D
  { position :: Point3D space
  , normalDirection :: Direction3D space
  }

instance Space.Coercion (SurfaceVertex3D space1) (SurfaceVertex3D space2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

{-# INLINE position #-}
position :: SurfaceVertex3D space -> Point3D space
position = (.position)

{-# INLINE normalDirection #-}
normalDirection :: SurfaceVertex3D space -> Direction3D space
normalDirection = (.normalDirection)

normalVector :: SurfaceVertex3D space -> Vector3D Unitless space
normalVector = Vector3D.unit . normalDirection
