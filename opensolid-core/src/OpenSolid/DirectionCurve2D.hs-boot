module OpenSolid.DirectionCurve2D
  ( DirectionCurve2D
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

newtype DirectionCurve2D space = DirectionCurve2D (VectorCurve2D Unitless space)

unsafe :: VectorCurve2D Unitless space -> DirectionCurve2D space
unwrap :: DirectionCurve2D space -> VectorCurve2D Unitless space
