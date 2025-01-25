module OpenSolid.Bounds2d
  ( Bounds2d
  , constant
  , xy
  )
where

import {-# SOURCE #-} OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)

type role Bounds2d nominal

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d ::
    Range units ->
    Range units ->
    Bounds2d (space @ units)

constant :: Point2d (space @ units) -> Bounds2d (space @ units)
xy :: forall space units. Range units -> Range units -> Bounds2d (space @ units)
