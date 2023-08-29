module Direction3d
  ( Direction3d
  , unsafe
  )
where

import OpenSolid
import {-# SOURCE #-} Vector3d (Vector3d)

type role Direction3d phantom

data Direction3d (space :: Type)

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
