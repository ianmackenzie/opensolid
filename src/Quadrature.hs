module Quadrature (parameterValues) where

import OpenSolid
import Qty qualified
import Range qualified

-- From https://en.wikipedia.org/wiki/Gaussian_quadrature#Gauss%E2%80%93Legendre_quadrature
parameterValues :: List Float
parameterValues =
  let x1 = Qty.sqrt ((3 / 7) - (2 / 7) * Qty.sqrt (6 / 5))
      x2 = Qty.sqrt ((3 / 7) + (2 / 7) * Qty.sqrt (6 / 5))
      normalize x = Range.interpolationParameter (Range.from -1.0 1.0) x
   in [ normalize -x2
      , normalize -x1
      , normalize x1
      , normalize x2
      ]
