module OpenSolid.Polyline
  ( Polyline
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

import OpenSolid.Bounds (Bounds, BoundsExists)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Line (Line (Line))
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point (Point, PointExists)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | A non-empty list of points joined by lines.
data Polyline dimension units space = Polyline
  { vertices :: NonEmpty (Point dimension units space)
  , length :: ~(Quantity units)
  }

instance FFI (Polyline 2 Meters Void) where
  representation = FFI.classRepresentation "Polyline2D"

instance FFI (Polyline 2 Unitless Void) where
  representation = FFI.classRepresentation "UvPolyline"

-- | Construct a polyline from its vertices.
fromVertices ::
  PointExists dimension units space =>
  NonEmpty (Point dimension units space) -> Polyline dimension units space
fromVertices givenVertices =
  Polyline
    { vertices = givenVertices
    , length = Quantity.sum (NonEmpty.successive Point.distanceFrom givenVertices)
    }

-- | Get the vertices of a polyline.
vertices :: Polyline dimension units space -> NonEmpty (Point dimension units space)
vertices = (.vertices)

-- | Get the number of vertices in a polyline.
numVertices :: Polyline dimension units space -> Int
numVertices = NonEmpty.length . vertices

-- | Get the start point of a polyline.
startPoint :: Polyline dimension units space -> Point dimension units space
startPoint = NonEmpty.first . vertices

-- | Get the end point of a polyline.
endPoint :: Polyline dimension units space -> Point dimension units space
endPoint = NonEmpty.last . vertices

-- | Get the start and end points of a polyline as a tuple.
endpoints ::
  Polyline dimension units space ->
  (Point dimension units space, Point dimension units space)
endpoints polyline = (startPoint polyline, endPoint polyline)

-- | Get the individual segments (edges) of a polyline.
segments :: Polyline dimension units space -> List (Line dimension units space)
segments polyline = NonEmpty.successive Line (vertices polyline)

-- | Get the total length of a polyline (the sum of the lengths of its segments).
length :: PointExists dimension units space => Polyline dimension units space -> Quantity units
length = (.length)

bounds ::
  BoundsExists dimension units space =>
  Polyline dimension units space ->
  Bounds dimension units space
bounds polyline = Bounds.hull (vertices polyline)

map ::
  PointExists dimension units2 space2 =>
  (Point dimension units1 space1 -> Point dimension units2 space2) ->
  Polyline dimension units1 space1 ->
  Polyline dimension units2 space2
map function polyline = fromVertices (NonEmpty.map function (vertices polyline))
