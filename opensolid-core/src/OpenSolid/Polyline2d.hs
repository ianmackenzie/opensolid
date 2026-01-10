module OpenSolid.Polyline2d
  ( Polyline2d (Polyline2d)
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

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Line2d (Line2d (Line2d))
import OpenSolid.Line2d qualified as Line2d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | A non-empty list of points joined by lines.
newtype Polyline2d units space
  = -- | Construct a polyline from its vertices.
    Polyline2d {vertices :: NonEmpty (Point2D units space)}

instance FFI (Polyline2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Polyline2d"

instance FFI (Polyline2d Unitless UvSpace) where
  representation = FFI.classRepresentation "UvPolyline"

-- | Get the vertices of a polyline.
vertices :: Polyline2d units space -> NonEmpty (Point2D units space)
vertices = (.vertices)

-- | Get the number of vertices in a polyline.
numVertices :: Polyline2d units space -> Int
numVertices polyline = NonEmpty.length (vertices polyline)

-- | Get the start point of a polyline.
startPoint :: Polyline2d units space -> Point2D units space
startPoint polyline = NonEmpty.first (vertices polyline)

-- | Get the end point of a polyline.
endPoint :: Polyline2d units space -> Point2D units space
endPoint polyline = NonEmpty.last (vertices polyline)

-- | Get the start and end points of a polyline as a tuple.
endpoints :: Polyline2d units space -> (Point2D units space, Point2D units space)
endpoints polyline = (startPoint polyline, endPoint polyline)

-- | Get the individual segments (edges) of a polyline.
segments :: Polyline2d units space -> List (Line2d units space)
segments polyline = NonEmpty.successive Line2d (vertices polyline)

-- | Get the total length of a polyline (the sum of the lengths of its segments).
length :: Polyline2d units space -> Quantity units
length polyline = Quantity.sumOf Line2d.length (segments polyline)

map ::
  (Point2D units1 space1 -> Point2D units2 space2) ->
  Polyline2d units1 space1 ->
  Polyline2d units2 space2
map function polyline = Polyline2d (NonEmpty.map function (vertices polyline))
