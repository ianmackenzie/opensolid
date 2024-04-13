module Bounds3d
  ( Bounds3d
  , constant
  , xyz
  )
where

import Bounds qualified
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)
import Range (Range)

type role Bounds3d phantom

data Bounds3d (coordinateSystem :: CoordinateSystem) where
  Bounds3d ::
    Range (Units coordinateSystem) ->
    Range (Units coordinateSystem) ->
    Range (Units coordinateSystem) ->
    Bounds3d coordinateSystem

instance Bounds.Interface (Bounds3d (space @ units))

constant :: Point3d (space @ units) -> Bounds3d (space @ units)
xyz :: Range units -> Range units -> Range units -> Bounds3d (space @ units)
