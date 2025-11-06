module OpenSolid.Intersects (Intersects (intersects)) where

import OpenSolid.Bootstrap
import OpenSolid.Tolerance (Tolerance)

class Intersects b a units => Intersects a b units | a b -> units where
  intersects :: Tolerance units => a -> b -> Bool

infix 4 `intersects`
