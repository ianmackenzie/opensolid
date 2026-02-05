module OpenSolid.Polyline2D
  ( Polyline2D (Polyline2D)
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
import OpenSolid.Line2D (Line2D (Line2D))
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.UvSpace (UvSpace)

-- | A non-empty list of points joined by lines.
newtype Polyline2D units space
  = -- | Construct a polyline from its vertices.
    Polyline2D {vertices :: NonEmpty (Point2D units space)}

instance FFI (Polyline2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Polyline2D"

instance FFI (Polyline2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvPolyline"

-- | Get the vertices of a polyline.
vertices :: Polyline2D units space -> NonEmpty (Point2D units space)
vertices = (.vertices)

-- | Get the number of vertices in a polyline.
numVertices :: Polyline2D units space -> Int
numVertices polyline = NonEmpty.length (vertices polyline)

-- | Get the start point of a polyline.
startPoint :: Polyline2D units space -> Point2D units space
startPoint polyline = NonEmpty.first (vertices polyline)

-- | Get the end point of a polyline.
endPoint :: Polyline2D units space -> Point2D units space
endPoint polyline = NonEmpty.last (vertices polyline)

-- | Get the start and end points of a polyline as a tuple.
endpoints :: Polyline2D units space -> (Point2D units space, Point2D units space)
endpoints polyline = (startPoint polyline, endPoint polyline)

-- | Get the individual segments (edges) of a polyline.
segments :: Polyline2D units space -> List (Line2D units space)
segments polyline = NonEmpty.successive Line2D (vertices polyline)

-- | Get the total length of a polyline (the sum of the lengths of its segments).
length :: Polyline2D units space -> Quantity units
length polyline = Quantity.sumOf Line2D.length (segments polyline)

map ::
  (Point2D units1 space1 -> Point2D units2 space2) ->
  Polyline2D units1 space1 ->
  Polyline2D units2 space2
map function polyline = Polyline2D (NonEmpty.map function (vertices polyline))
