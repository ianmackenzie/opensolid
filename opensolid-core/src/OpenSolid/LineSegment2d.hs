module OpenSolid.LineSegment2d
  ( LineSegment2d (LineSegment2d)
  , distanceTo
  )
where

import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified as Bounded2d
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vertex2d (Vertex2d, pattern Vertex2d)

data LineSegment2d vertex = LineSegment2d vertex vertex

instance HasField "start" (LineSegment2d vertex) vertex where
  getField (LineSegment2d start _) = start

instance HasField "end" (LineSegment2d vertex) vertex where
  getField (LineSegment2d _ end) = end

instance
  Vertex2d vertex (space @ units) =>
  Bounded2d (LineSegment2d vertex) (space @ units)
  where
  bounds (LineSegment2d (Vertex2d startPoint) (Vertex2d endPoint)) =
    Bounds2d.hull2 startPoint endPoint

instance
  Vertex2d vertex (space @ units) =>
  HasField "length" (LineSegment2d vertex) (Qty units)
  where
  getField (LineSegment2d (Vertex2d startPoint) (Vertex2d endPoint)) =
    Point2d.distanceFrom startPoint endPoint

distanceTo ::
  Vertex2d vertex (space @ units) =>
  Point2d (space @ units) ->
  LineSegment2d vertex ->
  Qty units
distanceTo p0 (LineSegment2d (Vertex2d p1) (Vertex2d p2)) = do
  let u = p0 - p1
  let v = p2 - p1
  let dSquared' = Vector2d.squaredMagnitude' v
  let dotProduct' = u `dot'` v
  if
    | dotProduct' <= Qty.zero -> Point2d.distanceFrom p1 p0
    | dotProduct' >= dSquared' -> Point2d.distanceFrom p2 p0
    | otherwise -> Qty.abs (u `cross'` v .!/! Qty.sqrt' dSquared')
