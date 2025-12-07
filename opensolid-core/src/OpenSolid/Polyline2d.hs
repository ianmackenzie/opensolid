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
import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.LineSegment2d qualified as LineSegment2d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Polymorphic.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | A non-empty list of points joined by line segments.
newtype Polyline2d units space
  = -- | Construct a polyline from its vertices.
    Polyline2d {vertices :: NonEmpty (Point2d units space)}

instance FFI (Polyline2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Polyline2d"

-- | Get the vertices of a polyline.
vertices :: Polyline2d units space -> NonEmpty (Point2d units space)
vertices = (.vertices)

-- | Get the number of vertices in a polyline.
numVertices :: Polyline2d units space -> Int
numVertices polyline = NonEmpty.length (vertices polyline)

-- | Get the start point of a polyline.
startPoint :: Polyline2d units space -> Point2d units space
startPoint polyline = NonEmpty.first (vertices polyline)

-- | Get the end point of a polyline.
endPoint :: Polyline2d units space -> Point2d units space
endPoint polyline = NonEmpty.last (vertices polyline)

-- | Get the start and end points of a polyline as a tuple.
endpoints :: Polyline2d units space -> (Point2d units space, Point2d units space)
endpoints polyline = (startPoint polyline, endPoint polyline)

-- | Get the individual segments (edges) of a polyline.
segments :: Polyline2d units space -> List (LineSegment2d units space)
segments polyline = NonEmpty.successive LineSegment2d (vertices polyline)

-- | Get the total length of a polyline (the sum of the lengths of its edges).
length :: Polyline2d units space -> Quantity units
length polyline = Quantity.sumOf LineSegment2d.length (segments polyline)

map ::
  (Point2d units1 space1 -> Point2d units2 space2) ->
  Polyline2d units1 space1 ->
  Polyline2d units2 space2
map function polyline = Polyline2d (NonEmpty.map function (vertices polyline))
