module Direction3d
  ( Direction3d
  , unsafe
  , unwrap
  )
where

import OpenSolid
import {-# SOURCE #-} Vector3d (Vector3d)

type role Direction3d phantom

data Direction3d (space :: Type)

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
unwrap :: Direction3d space -> Vector3d (space @ Unitless)
