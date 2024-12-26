module Bounds2d
  ( Bounds2d
  , constant
  , xy
  )
where

import Bounds qualified
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Point2d (Point2d)
import Range (Range)

type role Bounds2d phantom

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d ::
    Range (UnitsOf coordinateSystem) ->
    Range (UnitsOf coordinateSystem) ->
    Bounds2d coordinateSystem

instance Bounds.Interface (Bounds2d (space @ units))

constant :: Point2d (space @ units) -> Bounds2d (space @ units)
xy :: forall space units. Range units -> Range units -> Bounds2d (space @ units)
