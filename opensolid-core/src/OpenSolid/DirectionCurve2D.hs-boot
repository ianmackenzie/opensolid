module OpenSolid.DirectionCurve2D
  ( DirectionCurve2D
  , unsafe
  , unwrap
  , value
  , bounds
  , derivative
  )
where

import OpenSolid.Direction2D (Direction2D)
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

newtype DirectionCurve2D = DirectionCurve2D (VectorCurve2D Unitless)

unsafe :: VectorCurve2D Unitless -> DirectionCurve2D
unwrap :: DirectionCurve2D -> VectorCurve2D Unitless
value :: DirectionCurve2D -> Number -> Direction2D
bounds :: DirectionCurve2D -> Interval Unitless -> DirectionBounds2D
derivative :: DirectionCurve2D -> VectorCurve2D Unitless
