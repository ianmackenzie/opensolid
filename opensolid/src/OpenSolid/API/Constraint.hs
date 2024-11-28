module OpenSolid.API.Constraint (Constraint (..)) where

data Constraint
  = NoConstraint
  | ToleranceUnitless
  | ToleranceMeters
