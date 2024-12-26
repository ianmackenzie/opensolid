module OpenSolid.Vector2d
  ( Vector2d
  , xy
  , xComponent
  , yComponent
  , components
  , transformBy
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units

type role Vector2d phantom

data Vector2d (coordinateSystem :: CoordinateSystem)

instance Eq (Vector2d (space @ units))

instance Show (Vector2d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition (Vector2d (space @ units)) (Vector2d (space_ @ units_)) (Vector2d (space @ units))

instance Multiplication' (Qty units1) (Vector2d (space @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Vector2d (space @ units2)) (Vector2d (space @ units3))

instance Multiplication' (Vector2d (space @ units1)) (Qty units2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))

xy :: forall space units. Qty units -> Qty units -> Vector2d (space @ units)
xComponent :: Vector2d (space @ units) -> Qty units
yComponent :: Vector2d (space @ units) -> Qty units
components :: Vector2d (space @ units) -> (Qty units, Qty units)
transformBy :: Transform2d tag (space @ units1) -> Vector2d (space @ units2) -> Vector2d (space @ units2)
