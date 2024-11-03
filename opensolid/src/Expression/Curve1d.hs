module Expression.Curve1d (constant) where

import Expression (Expression)
import Expression qualified
import OpenSolid

constant :: Qty units -> Expression Float (Qty units)
constant = Expression.constant
