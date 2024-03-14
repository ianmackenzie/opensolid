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

instance Eq (Vector2d (space @ units))

instance Show (Vector2d (space @ units))

xy :: Qty units -> Qty units -> Vector2d (space @ units)
xComponent :: Vector2d (space @ units) -> Qty units
yComponent :: Vector2d (space @ units) -> Qty units
