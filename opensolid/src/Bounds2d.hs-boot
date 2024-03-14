module Bounds2d
  ( Bounds2d (Bounds2d)
  , constant
  , xy
  )
where

import Bounds qualified
import CoordinateSystem qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Range (Range)

type role Bounds2d phantom

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d ::
    Range (CoordinateSystem.Units coordinateSystem) ->
    Range (CoordinateSystem.Units coordinateSystem) ->
    Bounds2d coordinateSystem

instance Bounds.Interface (Bounds2d (space @ units))

constant :: Point2d (space @ units) -> Bounds2d (space @ units)
xy :: Range units -> Range units -> Bounds2d (space @ units)
