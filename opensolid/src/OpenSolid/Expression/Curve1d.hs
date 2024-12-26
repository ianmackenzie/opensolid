module OpenSolid.Expression.Curve1d (constant) where

import OpenSolid.Prelude
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression

constant :: Qty units -> Expression Float (Qty units)
constant = Expression.constant
