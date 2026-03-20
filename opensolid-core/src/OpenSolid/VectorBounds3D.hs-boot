module OpenSolid.VectorBounds3D
  ( VectorBounds3D
  , center
  , diameter
  , normalize
  , isResolved
  , areDistinct
  , areIndependent
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds3D)
import {-# SOURCE #-} OpenSolid.Vector3D (Vector3D)

center :: VectorBounds3D units space -> Vector3D units space
diameter :: VectorBounds3D units space -> Quantity units
normalize :: VectorBounds3D units space -> VectorBounds3D Unitless space
isResolved :: VectorBounds3D units space -> Bool
areDistinct :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
areIndependent :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
