module OpenSolid.VectorCurve.Nondegenerate
  ( Nondegenerate
  , unsafe
  , curve
  , magnitude
  , normalized
  , squaredMagnitude_
  , squaredMagnitude
  , direction
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type role Nondegenerate nominal nominal nominal

data Nondegenerate (dimension :: Natural) (units :: Type) (space :: Type)

unsafe ::
  VectorCurve.Exists dimension units space =>
  VectorCurve dimension units space -> Nondegenerate dimension units space
curve :: Nondegenerate dimension units space -> VectorCurve dimension units space
magnitude :: Nondegenerate dimension units space -> Curve1D.Nondegenerate units
normalized :: Nondegenerate dimension units space -> VectorCurve dimension Unitless space
squaredMagnitude_ ::
  VectorCurve.Exists dimension units space =>
  Nondegenerate dimension units space ->
  Curve1D.Nondegenerate (units ?*? units)
squaredMagnitude ::
  (VectorCurve.Exists dimension units1 space, Units.Squared units1 units2) =>
  Nondegenerate dimension units1 space ->
  Curve1D.Nondegenerate units2
direction ::
  DirectionCurve.Exists dimension space =>
  Nondegenerate dimension units space ->
  DirectionCurve dimension space
