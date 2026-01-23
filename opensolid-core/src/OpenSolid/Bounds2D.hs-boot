module OpenSolid.Bounds2D
  ( Bounds2D
  , aggregate2
  , contains
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D)

aggregate2 :: Bounds2D units space -> Bounds2D units space -> Bounds2D units space
contains :: Bounds2D units space -> Bounds2D units space -> Bool
