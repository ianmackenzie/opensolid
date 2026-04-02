module OpenSolid.Bounds2D
  ( Bounds2D
  , aggregate2
  , contains
  , diameter
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D)

aggregate2 :: Bounds2D units -> Bounds2D units -> Bounds2D units
contains :: Bounds2D units -> Bounds2D units -> Bool
diameter :: Bounds2D units -> Quantity units
