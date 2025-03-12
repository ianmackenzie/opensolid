module OpenSolid.LineSegment2d
  ( LineSegment2d (..)
  , length
  , distanceTo
  )
where

import OpenSolid.Bounds2d (Bounded2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vertex2d (Vertex2d, pattern Vertex2d)

data LineSegment2d vertex = LineSegment2d
  { start :: vertex
  , end :: vertex
  }

instance
  Vertex2d vertex (space @ units) =>
  Bounded2d (LineSegment2d vertex) (space @ units)
  where
  bounds (LineSegment2d (Vertex2d startPoint) (Vertex2d endPoint)) =
    Bounds2d.hull2 startPoint endPoint

length :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Qty units
length (LineSegment2d (Vertex2d startPoint) (Vertex2d endPoint)) =
  Point2d.distanceFrom startPoint endPoint

distanceTo ::
  Vertex2d vertex (space @ units) =>
  Point2d (space @ units) ->
  LineSegment2d vertex ->
  Qty units
distanceTo p0 LineSegment2d{start = Vertex2d p1, end = Vertex2d p2} = do
  let v = p2 - p1
  let u = p0 - p1
  let dSquared' = Vector2d.squaredMagnitude' v
  let dotProduct' = u `dot'` v
  if
    | dotProduct' <= Qty.zero -> Point2d.distanceFrom p1 p0
    | dotProduct' >= dSquared' -> Point2d.distanceFrom p2 p0
    | otherwise -> Qty.abs (u `cross'` v .!/! Qty.sqrt' dSquared')
