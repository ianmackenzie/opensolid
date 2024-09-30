module Jit.VectorExpression2d (VectorExpression2d (VectorExpression2d)) where

import {-# SOURCE #-} Jit.Expression (Expression)

data VectorExpression2d parameterization
  = VectorExpression2d (Expression parameterization) (Expression parameterization)
