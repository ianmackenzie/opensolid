module OpenSolid.Expression.Surface1D (constant) where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

constant :: Number -> Expression UvPoint Number
constant = Expression.constant
