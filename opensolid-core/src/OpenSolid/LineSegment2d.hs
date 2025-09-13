module OpenSolid.LineSegment2d
  ( LineSegment2d (LineSegment2d)
  , startVertex
  , endVertex
  , startPoint
  , endPoint
  , length
  , bounds
  , distanceTo
  )
where

import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified as Bounded2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

data LineSegment2d vertex = LineSegment2d vertex vertex

instance HasField "startVertex" (LineSegment2d vertex) vertex where
  getField = startVertex

instance HasField "endVertex" (LineSegment2d vertex) vertex where
  getField = endVertex

instance
  Vertex2d vertex (space @ units) =>
  HasField "startPoint" (LineSegment2d vertex) (Point2d (space @ units))
  where
  getField = startPoint

instance
  Vertex2d vertex (space @ units) =>
  HasField "endPoint" (LineSegment2d vertex) (Point2d (space @ units))
  where
  getField = endPoint

instance
  Vertex2d vertex (space @ units) =>
  HasField "length" (LineSegment2d vertex) (Qty units)
  where
  getField = length

instance
  Vertex2d vertex (space @ units) =>
  HasField "bounds" (LineSegment2d vertex) (Bounds2d (space @ units))
  where
  getField = bounds

instance
  Vertex2d vertex (space @ units) =>
  Bounded2d (LineSegment2d vertex) (space @ units)
  where
  bounds = bounds

{-# INLINE startVertex #-}
startVertex :: LineSegment2d vertex -> vertex
startVertex (LineSegment2d v1 _) = v1

{-# INLINE endVertex #-}
endVertex :: LineSegment2d vertex -> vertex
endVertex (LineSegment2d _ v2) = v2

{-# INLINE startPoint #-}
startPoint :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Point2d (space @ units)
startPoint (LineSegment2d v1 _) = Vertex2d.position v1

{-# INLINE endPoint #-}
endPoint :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Point2d (space @ units)
endPoint (LineSegment2d _ v2) = Vertex2d.position v2

length :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Qty units
length segment = Point2d.distanceFrom segment.startPoint segment.endPoint

bounds :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Bounds2d (space @ units)
bounds segment = Bounds2d.hull2 segment.startPoint segment.endPoint

distanceTo ::
  Vertex2d vertex (space @ units) =>
  Point2d (space @ units) ->
  LineSegment2d vertex ->
  Qty units
distanceTo p0 segment = do
  let p1 = segment.startPoint
  let p2 = segment.endPoint
  let u = p0 - p1
  let v = p2 - p1
  let dSquared' = Vector2d.squaredMagnitude' v
  let dotProduct' = u `dot'` v
  if
    | dotProduct' <= Qty.zero -> Point2d.distanceFrom p1 p0
    | dotProduct' >= dSquared' -> Point2d.distanceFrom p2 p0
    | otherwise -> Qty.abs (Units.simplify (u `cross'` v ./. Qty.sqrt' dSquared'))
