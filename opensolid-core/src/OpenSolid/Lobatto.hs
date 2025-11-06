module OpenSolid.Lobatto (p2, p3, w1, w2, w3, w4, estimate) where

import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding (fromInteger)
import Prelude ((*), (+), (-), (/))

p2 :: Number
p2 = 0.5 - 1 / (2 * Number.sqrt 5)

p3 :: Number
p3 = 0.5 + 1 / (2 * Number.sqrt 5)

w1 :: Number
w1 = 1 / 12

w2 :: Number
w2 = 5 / 12

w3 :: Number
w3 = 5 / 12

w4 :: Number
w4 = 1 / 12

estimate :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Quantity units
estimate y1 y2 y3 y4 = w1 .*. y1 .+. w2 .*. y2 .+. w3 .*. y3 .+. w4 .*. y4
