module OpenSolid.API.Constraint (Constraint (..)) where

import Data.Kind qualified
import OpenSolid
import Units (Meters)

data Constraint (constraint :: Data.Kind.Constraint) where
  N :: Constraint ()
  F :: Constraint (Tolerance Unitless)
  L :: Constraint (Tolerance Meters)
