module OpenSolid.Bounded2d (Bounded2d (bounds)) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2d, Point2d)

class Bounded2d a (coordinateSystem :: CoordinateSystem) | a -> coordinateSystem where
  bounds :: a -> Bounds2d coordinateSystem

instance Bounded2d (Point2d (space @ units)) (space @ units) where
  bounds = Bounds2d.constant

instance Bounded2d (Bounds2d (space @ units)) (space @ units) where
  bounds = identity
