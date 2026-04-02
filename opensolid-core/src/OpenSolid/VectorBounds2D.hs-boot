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

center :: VectorBounds2D units -> Vector2D units
diameter :: VectorBounds2D units -> Quantity units
normalize :: VectorBounds2D units -> VectorBounds2D Unitless
isResolved :: VectorBounds2D units -> Bool
areDistinct :: VectorBounds2D units -> VectorBounds2D units -> Bool
areIndependent :: VectorBounds2D units -> VectorBounds2D units -> Bool
