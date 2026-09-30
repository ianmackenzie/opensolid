module OpenSolid.Polyline2D
  ( Polyline2D
  , fromVertices
  , vertices
  , numVertices
  , startPoint
  , endPoint
  , endpoints
  , segments
  , length
  , bounds
  , map
  )
where

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Line2D (Line2D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Polyline (Polyline)
import OpenSolid.Polyline qualified as Polyline
import OpenSolid.Prelude

-- | A non-empty list of points joined by lines.
type Polyline2D units = Polyline 2 units Void

-- | Construct a polyline from its vertices.
fromVertices :: NonEmpty (Point2D units) -> Polyline2D units
fromVertices = Polyline.fromVertices

-- | Get the vertices of a polyline.
vertices :: Polyline2D units -> NonEmpty (Point2D units)
vertices = Polyline.vertices

-- | Get the number of vertices in a polyline.
numVertices :: Polyline2D units -> Int
numVertices = Polyline.numVertices

-- | Get the start point of a polyline.
startPoint :: Polyline2D units -> Point2D units
startPoint = Polyline.startPoint

-- | Get the end point of a polyline.
endPoint :: Polyline2D units -> Point2D units
endPoint = Polyline.endPoint

-- | Get the start and end points of a polyline as a tuple.
endpoints :: Polyline2D units -> (Point2D units, Point2D units)
endpoints = Polyline.endpoints

-- | Get the individual segments (edges) of a polyline.
segments :: Polyline2D units -> List (Line2D units)
segments = Polyline.segments

-- | Get the total length of a polyline (the sum of the lengths of its segments).
length :: Polyline2D units -> Quantity units
length = Polyline.length

-- | Get the bounding box of a polyline.
bounds :: Polyline2D units -> Bounds2D units
bounds = Polyline.bounds

map :: (Point2D units1 -> Point2D units2) -> Polyline2D units1 -> Polyline2D units2
map = Polyline.map
