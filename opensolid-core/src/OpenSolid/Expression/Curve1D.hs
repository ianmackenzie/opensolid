module OpenSolid.Expression.Curve1D (constant) where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude

constant :: Quantity units -> Expression Number (Quantity units)
constant = Expression.constant
