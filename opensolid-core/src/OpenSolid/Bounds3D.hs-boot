module OpenSolid.Bounds3D
  ( Bounds3D
  , aggregate2
  , contains
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D)

aggregate2 :: Bounds3D space -> Bounds3D space -> Bounds3D space
contains :: Bounds3D space -> Bounds3D space -> Bool
