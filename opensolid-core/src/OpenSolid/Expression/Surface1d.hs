module OpenSolid.Expression.Surface1d (constant) where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

constant :: Float -> Expression UvPoint Float
constant = Expression.constant
