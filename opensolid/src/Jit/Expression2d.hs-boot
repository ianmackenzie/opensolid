module Jit.Expression2d (Expression2d (Expression2d)) where

import {-# SOURCE #-} Jit.Expression (Expression)

data Expression2d parameterization
  = Expression2d (Expression parameterization) (Expression parameterization)
