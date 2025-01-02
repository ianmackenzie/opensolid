module OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d)) where

import OpenSolid.Prelude
import OpenSolid.Range (Range)

type role VectorBounds2d nominal

data VectorBounds2d (coordinateSystem :: CoordinateSystem) where
  VectorBounds2d :: Range units -> Range units -> VectorBounds2d (space @ units)
