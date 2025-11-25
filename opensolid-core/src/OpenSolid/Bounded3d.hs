module OpenSolid.Bounded3d (Bounded3d (bounds)) where

import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3d, Point3d)

class Bounded3d a space | a -> space where
  bounds :: a -> Bounds3d space Meters

instance meters ~ Meters => Bounded3d (Point3d space meters) space where
  bounds = Bounds3d.constant

instance meters ~ Meters => Bounded3d (Bounds3d space meters) space where
  bounds = id
