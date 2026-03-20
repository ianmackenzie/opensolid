module OpenSolid.VectorBounds2D
  ( VectorBounds2D
  , center
  , diameter
  , normalize
  , isResolved
  , areDistinct
  , areIndependent
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (VectorBounds2D)
import {-# SOURCE #-} OpenSolid.Vector2D (Vector2D)

center :: VectorBounds2D units space -> Vector2D units space
diameter :: VectorBounds2D units space -> Quantity units
normalize :: VectorBounds2D units space -> VectorBounds2D Unitless space
isResolved :: VectorBounds2D units space -> Bool
areDistinct :: VectorBounds2D units space -> VectorBounds2D units space -> Bool
areIndependent :: VectorBounds2D units space -> VectorBounds2D units space -> Bool
