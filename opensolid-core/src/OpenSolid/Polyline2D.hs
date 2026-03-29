module OpenSolid.Polyline2D
  ( Polyline2D
  , pattern Polyline2D
  , vertices
  , numVertices
  , startPoint
  , endPoint
  , endpoints
  , segments
  , length
  , map
  )
where

import OpenSolid.Line2D (Line2D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Polyline (Polyline (Polyline))
import OpenSolid.Polyline qualified as Polyline
import OpenSolid.Prelude

-- | A non-empty list of points joined by lines.
type Polyline2D units space = Polyline 2 units space

-- | Construct a polyline from its vertices.
pattern Polyline2D :: NonEmpty (Point2D units space) -> Polyline2D units space
pattern Polyline2D vertices_ = Polyline vertices_

-- | Get the vertices of a polyline.
vertices :: Polyline2D units space -> NonEmpty (Point2D units space)
vertices = Polyline.vertices

-- | Get the number of vertices in a polyline.
numVertices :: Polyline2D units space -> Int
numVertices = Polyline.numVertices

-- | Get the start point of a polyline.
startPoint :: Polyline2D units space -> Point2D units space
startPoint = Polyline.startPoint

-- | Get the end point of a polyline.
endPoint :: Polyline2D units space -> Point2D units space
endPoint = Polyline.endPoint

-- | Get the start and end points of a polyline as a tuple.
endpoints :: Polyline2D units space -> (Point2D units space, Point2D units space)
endpoints = Polyline.endpoints

-- | Get the individual segments (edges) of a polyline.
segments :: Polyline2D units space -> List (Line2D units space)
segments = Polyline.segments

-- | Get the total length of a polyline (the sum of the lengths of its segments).
length :: Polyline2D units space -> Quantity units
length = Polyline.length

map ::
  (Point2D units1 space1 -> Point2D units2 space2) ->
  Polyline2D units1 space1 ->
  Polyline2D units2 space2
map = Polyline.map
