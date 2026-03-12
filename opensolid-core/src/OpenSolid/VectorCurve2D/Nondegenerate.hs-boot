module OpenSolid.VectorCurve2D.Nondegenerate (Nondegenerate) where

import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

type role Nondegenerate nominal nominal

data Nondegenerate (units :: Type) (space :: Type)

instance HasUnits (Nondegenerate units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Nondegenerate units1 space1) (Nondegenerate units2 space2)
