module OpenSolid.VectorCurve.Nondegenerate
  ( Nondegenerate
  , unsafe
  , magnitude
  , normalized
  , direction
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type role Nondegenerate nominal nominal nominal

data Nondegenerate (dimension :: Natural) (units :: Type) (space :: Type)

unsafe ::
  VectorCurve.Exists dimension units space =>
  VectorCurve dimension units space -> Nondegenerate dimension units space
magnitude :: Nondegenerate dimension units space -> Curve1D.WithNoInteriorZeros units
normalized :: Nondegenerate dimension units space -> VectorCurve dimension Unitless space
direction ::
  DirectionCurve.Exists dimension space =>
  Nondegenerate dimension units space ->
  DirectionCurve dimension space
