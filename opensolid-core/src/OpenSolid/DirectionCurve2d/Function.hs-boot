module OpenSolid.DirectionCurve2d.Function
  ( Function
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve2d.Function qualified as VectorCurve2d.Function

newtype Function space = Function (VectorCurve2d.Function.Function (space @ Unitless))

unsafe :: VectorCurve2d.Function.Function (space @ Unitless) -> Function space
unwrap :: Function space -> VectorCurve2d.Function.Function (space @ Unitless)
