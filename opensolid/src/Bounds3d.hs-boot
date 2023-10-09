module Bounds3d
  ( Bounds3d (Bounds3d)
  , constant
  )
where

import Bounds (IsBounds)
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)
import Range (Range)

type role Bounds3d nominal

data Bounds3d (coordinateSystem :: CoordinateSystem) where
  Bounds3d :: Range units -> Range units -> Range units -> Bounds3d (space @ units)

instance IsBounds (Bounds3d (space @ units))

constant :: Point3d (space @ units) -> Bounds3d (space @ units)
