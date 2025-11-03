module OpenSolid.Expression.Curve1d (constant) where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude

constant :: Quantity units -> Expression Float (Quantity units)
constant = Expression.constant
