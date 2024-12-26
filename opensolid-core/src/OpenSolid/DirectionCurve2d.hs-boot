module OpenSolid.DirectionCurve2d
  ( DirectionCurve2d
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))

unsafe :: VectorCurve2d (space @ Unitless) -> DirectionCurve2d space
unwrap :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
