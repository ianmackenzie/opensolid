module Vector3d
  ( Vector3d
  , xyz
  , IsZero (IsZero)
  , direction
  )
where

import {-# SOURCE #-} Direction3d (Direction3d)
import OpenSolid

type role Vector3d phantom

data Vector3d (coordinateSystem :: CoordinateSystem)

xyz :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)

data IsZero = IsZero

direction :: Vector3d (space @ units) -> Result IsZero (Direction3d space)
