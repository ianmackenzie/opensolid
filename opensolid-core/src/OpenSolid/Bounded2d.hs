module OpenSolid.Bounded2d (Bounded2d (bounds)) where

import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2d, Point2d)

class Bounded2d a units space | a -> space, a -> units where
  bounds :: a -> Bounds2d units space

instance Bounded2d (Point2d units space) units space where
  bounds = Bounds2d.constant

instance Bounded2d (Bounds2d units space) units space where
  bounds = id
