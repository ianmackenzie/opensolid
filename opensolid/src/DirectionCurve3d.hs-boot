module DirectionCurve3d
  ( DirectionCurve3d
  , unsafe
  , unwrap
  )
where

import OpenSolid
import {-# SOURCE #-} VectorCurve3d (VectorCurve3d)

newtype DirectionCurve3d space = DirectionCurve3d (VectorCurve3d (space @ Unitless))

unsafe :: VectorCurve3d (space @ Unitless) -> DirectionCurve3d space
unwrap :: DirectionCurve3d space -> VectorCurve3d (space @ Unitless)
