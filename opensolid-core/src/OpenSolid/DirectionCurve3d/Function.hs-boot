module OpenSolid.DirectionCurve3d.Function
  ( Function
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve3d.Function qualified as VectorCurve3d.Function

newtype Function space = Function (VectorCurve3d.Function.Function (space @ Unitless))

unsafe :: VectorCurve3d.Function.Function (space @ Unitless) -> Function space
unwrap :: Function space -> VectorCurve3d.Function.Function (space @ Unitless)
