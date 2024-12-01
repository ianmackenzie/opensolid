module OpenSolid.API.Constraint (Constraint (..)) where

data Constraint
  = ToleranceUnitless
  | ToleranceMeters
  | ToleranceRadians
