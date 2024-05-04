module Point2d
  ( Point2d
  , origin
  , xy
  , coordinates
  , transformBy
  )
where

import OpenSolid
import {-# SOURCE #-} Transform2d (Transform2d)
import Units qualified
import {-# SOURCE #-} Vector2d (Vector2d)

type role Point2d phantom

data Point2d (coordinateSystem :: CoordinateSystem)

instance Show (Point2d (space @ units))

instance HasUnits (Point2d (space @ units))

instance space ~ space' => Units.Coercion (Point2d (space @ units1)) (Point2d (space' @ units2))

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point2d (space @ units))
    (Point2d (space' @ units'))
    (Vector2d (space @ units))

origin :: Point2d (space @ units)
xy :: Qty units -> Qty units -> Point2d (space @ units)
coordinates :: Point2d (space @ units) -> (Qty units, Qty units)
transformBy :: Transform2d a (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)

instance
  (units ~ units', space ~ space') =>
  Addition
    (Point2d (space @ units))
    (Vector2d (space' @ units'))
    (Point2d (space @ units))

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point2d (space @ units))
    (Vector2d (space' @ units'))
    (Point2d (space @ units))
