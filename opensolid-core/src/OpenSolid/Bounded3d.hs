module OpenSolid.Bounded3d (Bounded3d (bounds)) where

import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3d, Point3d)

class Bounded3d a (coordinateSystem :: CoordinateSystem) | a -> coordinateSystem where
  bounds :: a -> Bounds3d coordinateSystem

instance Bounded3d (Point3d (space @ units)) (space @ units) where
  bounds = Bounds3d.constant

instance Bounded3d (Bounds3d (space @ units)) (space @ units) where
  bounds = id
