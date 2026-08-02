module OpenSolid.Vector3D.Nonzero (normalize, direction) where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D

normalize :: Nonzero (Vector3D units space) -> Nonzero (Vector3D Unitless space)
normalize (Nonzero vector) = Nonzero (vector / Vector3D.magnitude vector)

direction :: Nonzero (Vector3D units space) -> Direction3D space
direction (Nonzero vector) = Direction3D.unsafe (vector / Vector3D.magnitude vector)
