module OpenSolid.Intersects (Intersects (intersects)) where

import OpenSolid.Tolerance (Tolerance)
import Prelude (Bool)

class Intersects b a units => Intersects a b units | a b -> units where
  intersects :: Tolerance units => a -> b -> Bool

infix 4 `intersects`
