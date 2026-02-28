module OpenSolid.VectorBounds3D
  ( VectorBounds3D
  , center
  , isResolved
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds3D)
import {-# SOURCE #-} OpenSolid.Vector3D (Vector3D)

center :: VectorBounds3D units space -> Vector3D units space
isResolved :: VectorBounds3D units space -> Bool
