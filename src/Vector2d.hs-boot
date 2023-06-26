module Vector2d
  ( Vector2d
  , xy
  , IsZero (IsZero)
  , direction
  )
where

import {-# SOURCE #-} Direction2d (Direction2d)
import OpenSolid

type role Vector2d phantom

data Vector2d (coordinateSystem :: CoordinateSystem)

xy :: Qty units -> Qty units -> Vector2d (space @ units)

data IsZero = IsZero

direction :: Tolerance units => Vector2d (space @ units) -> Result IsZero (Direction2d space)
