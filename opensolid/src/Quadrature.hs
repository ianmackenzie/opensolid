module Quadrature (t1, t2, t3, t4, t5, points) where

import OpenSolid

-- Parameter values are Gaussian quadrature abscissae,
-- taken from https://pomax.github.io/bezierinfo/legendre-gauss.html#n5
-- and converted to [0, 1]

t1 :: Float
t1 = 0.04691007703066802

t2 :: Float
t2 = 0.23076534494715845

t3 :: Float
t3 = 0.5

t4 :: Float
t4 = 0.7692346550528415

t5 :: Float
t5 = 0.9530899229693319

points :: List Float
points = [t1, t2, t3, t4, t5]
