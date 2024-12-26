module OpenSolid.Expression.Curve1d (constant) where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude

constant :: Qty units -> Expression Float (Qty units)
constant = Expression.constant
