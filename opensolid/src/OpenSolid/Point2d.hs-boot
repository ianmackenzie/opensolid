module OpenSolid.Point2d
  ( Point2d
  , origin
  , xy
  , coordinates
  , transformBy
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Vector2d (Vector2d)
import {-# SOURCE #-} OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units

type role Point2d phantom

data Point2d (coordinateSystem :: CoordinateSystem)

instance Eq (Point2d (space @ units))

instance Show (Point2d (space @ units))

instance HasUnits (Point2d (space @ units))

instance space1 ~ space2 => Units.Coercion (Point2d (space1 @ unitsA)) (Point2d (space2 @ unitsB))

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (Vector2d (space1 @ units1))

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))

origin :: Point2d (space @ units)
xy :: forall space units. Qty units -> Qty units -> Point2d (space @ units)
coordinates :: Point2d (space @ units) -> (Qty units, Qty units)
transformBy :: Transform2d tag (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
