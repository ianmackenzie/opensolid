module OpenSolid.VectorBounds2D
  ( VectorBounds2D
  , center
  , isResolved
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2D)
import {-# SOURCE #-} OpenSolid.Vector2D (Vector2D)

center :: VectorBounds2D units space -> Vector2D units space
isResolved :: VectorBounds2D units space -> Bool
