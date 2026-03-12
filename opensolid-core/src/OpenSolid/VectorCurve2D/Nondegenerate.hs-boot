module OpenSolid.VectorCurve2D.Nondegenerate
  ( Nondegenerate
  , curve
  , magnitude
  , normalize
  , squaredMagnitude_
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type role Nondegenerate nominal nominal

data Nondegenerate (units :: Type) (space :: Type)

instance HasUnits (Nondegenerate units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Nondegenerate units1 space1) (Nondegenerate units2 space2)

curve :: Nondegenerate units space -> VectorCurve2D units space
magnitude :: Nondegenerate units space -> Curve1D.Nondegenerate units
normalize :: Nondegenerate units space -> VectorCurve2D Unitless space
squaredMagnitude_ :: Nondegenerate units space -> Curve1D.Nondegenerate (units ?*? units)
