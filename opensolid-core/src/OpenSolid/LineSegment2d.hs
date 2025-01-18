module OpenSolid.LineSegment2d
  ( LineSegment2d (..)
  , length
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Vertex2d (Vertex2d, pattern Vertex2d)

data LineSegment2d vertex = LineSegment2d
  { start :: vertex
  , end :: vertex
  }

instance
  Vertex2d vertex (space @ units) =>
  Bounded (LineSegment2d vertex) (Bounds2d (space @ units))
  where
  bounds (LineSegment2d (Vertex2d startPoint) (Vertex2d endPoint)) =
    Bounds2d.hull2 startPoint endPoint

length :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Qty units
length (LineSegment2d (Vertex2d startPoint) (Vertex2d endPoint)) =
  Point2d.distanceFrom startPoint endPoint
