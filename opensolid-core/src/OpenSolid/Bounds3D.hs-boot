module OpenSolid.Bounds3D
  ( Bounds3D
  , aggregate2
  , contains
  , diameter
  )
where

import OpenSolid.Length (Length)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D)

aggregate2 :: Bounds3D space -> Bounds3D space -> Bounds3D space
contains :: Bounds3D space -> Bounds3D space -> Bool
diameter :: Bounds3D space -> Length
