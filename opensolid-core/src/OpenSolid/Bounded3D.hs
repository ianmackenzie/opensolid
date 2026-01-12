module OpenSolid.Bounded3D (Bounded3D (bounds)) where

import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D, Point3D)

class Bounded3D a space | a -> space where
  bounds :: a -> Bounds3D space

instance Bounded3D (Point3D space) space where
  bounds = Bounds3D.constant

instance Bounded3D (Bounds3D space) space where
  bounds = id
