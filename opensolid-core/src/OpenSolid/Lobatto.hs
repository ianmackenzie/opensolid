module OpenSolid.Lobatto (p2, p3, w1, w2, w3, w4, estimate) where

import OpenSolid.Float qualified as Float
import OpenSolid.Prelude

p2 :: Float
p2 = 0.5 - 1.0 / (2.0 * Float.sqrt 5.0)

p3 :: Float
p3 = 0.5 + 1.0 / (2.0 * Float.sqrt 5.0)

w1 :: Float
w1 = 1.0 / 12.0

w2 :: Float
w2 = 5.0 / 12.0

w3 :: Float
w3 = 5.0 / 12.0

w4 :: Float
w4 = 1.0 / 12.0

estimate :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Quantity units
estimate y1 y2 y3 y4 = w1 * y1 + w2 * y2 + w3 * y3 + w4 * y4
