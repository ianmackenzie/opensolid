module OpenSolid.VectorBounds2D
  ( VectorBounds2D
  , center
  )
where

import OpenSolid.Primitives (VectorBounds2D)
import {-# SOURCE #-} OpenSolid.Vector2D (Vector2D)

center :: VectorBounds2D units space -> Vector2D units space
