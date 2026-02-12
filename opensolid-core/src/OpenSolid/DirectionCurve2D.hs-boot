module OpenSolid.DirectionCurve2D
  ( DirectionCurve2D
  , unsafe
  , unwrap
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Direction2D (Direction2D)
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

newtype DirectionCurve2D space = DirectionCurve2D (VectorCurve2D Unitless space)

unsafe :: VectorCurve2D Unitless space -> DirectionCurve2D space
unwrap :: DirectionCurve2D space -> VectorCurve2D Unitless space
evaluate :: DirectionCurve2D space -> Number -> Direction2D space
evaluateBounds :: DirectionCurve2D space -> Interval Unitless -> DirectionBounds2D space
derivative :: DirectionCurve2D space -> VectorCurve2D Unitless space
