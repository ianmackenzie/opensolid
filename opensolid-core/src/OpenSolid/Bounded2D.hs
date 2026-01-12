module OpenSolid.Bounded2D (Bounded2D (bounds)) where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D, Point2D)

class Bounded2D a units space | a -> space, a -> units where
  bounds :: a -> Bounds2D units space

instance Bounded2D (Point2D units space) units space where
  bounds = Bounds2D.constant

instance Bounded2D (Bounds2D units space) units space where
  bounds = id
