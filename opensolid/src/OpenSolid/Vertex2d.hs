module OpenSolid.Vertex2d (Vertex2d (..)) where

import OpenSolid.Prelude
import OpenSolid.Point2d (Point2d)

class Vertex2d vertex (coordinateSystem :: CoordinateSystem) | vertex -> coordinateSystem where
  position :: vertex -> Point2d coordinateSystem

instance Vertex2d (Point2d (space @ units)) (space @ units) where
  position = identity
