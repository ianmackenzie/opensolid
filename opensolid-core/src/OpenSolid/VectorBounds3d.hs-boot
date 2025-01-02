module OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d)) where

import OpenSolid.Prelude
import OpenSolid.Range (Range)

type role VectorBounds3d nominal

data VectorBounds3d (coordinateSystem :: CoordinateSystem) where
  VectorBounds3d :: Range units -> Range units -> Range units -> VectorBounds3d (space @ units)
