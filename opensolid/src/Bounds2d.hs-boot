module Bounds2d
  ( Bounds2d (Bounds2d)
  , constant
  )
where

import Bounds (IsBounds)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Range (Range)

type role Bounds2d nominal

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d :: Range units -> Range units -> Bounds2d (space @ units)

instance IsBounds (Bounds2d (space @ units))

constant :: Point2d (space @ units) -> Bounds2d (space @ units)
