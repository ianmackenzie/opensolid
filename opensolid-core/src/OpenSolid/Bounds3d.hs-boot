module OpenSolid.Bounds3d
  ( Bounds3d
  , constant
  , xyz
  )
where

import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)

type role Bounds3d nominal

data Bounds3d (coordinateSystem :: CoordinateSystem) where
  Bounds3d ::
    Range units ->
    Range units ->
    Range units ->
    Bounds3d (space @ units)

instance Bounds.Interface (Bounds3d (space @ units))

constant :: Point3d (space @ units) -> Bounds3d (space @ units)
xyz :: Range units -> Range units -> Range units -> Bounds3d (space @ units)
