module Vector2d
  ( Vector2d
  , xy
  , xComponent
  , yComponent
  )
where

import OpenSolid

type role Vector2d phantom

data Vector2d (coordinateSystem :: CoordinateSystem)

instance Eq (Vector2d coordinateSystem)

instance Show (Vector2d coordinateSystem)

xy :: Qty units -> Qty units -> Vector2d (space @ units)
xComponent :: Vector2d (space @ units) -> Qty units
yComponent :: Vector2d (space @ units) -> Qty units
