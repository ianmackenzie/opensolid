module OpenSolid.VectorBounds3D
  ( VectorBounds3D
  , center
  )
where

import OpenSolid.Primitives (VectorBounds3D)
import {-# SOURCE #-} OpenSolid.Vector3D (Vector3D)

center :: VectorBounds3D units space -> Vector3D units space
