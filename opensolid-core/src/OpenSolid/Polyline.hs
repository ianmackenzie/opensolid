module OpenSolid.Polyline
  ( Polyline (Polyline)
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

import Data.Void (Void)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Line (Line (Line))
import OpenSolid.Line qualified as Line
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | A non-empty list of points joined by lines.
newtype Polyline dimension units space
  = -- | Construct a polyline from its vertices.
    Polyline (NonEmpty (Point dimension units space))

instance FFI (Polyline 2 Meters Void) where
  representation = FFI.classRepresentation "Polyline2D"

instance FFI (Polyline 2 Unitless Void) where
  representation = FFI.classRepresentation "UvPolyline"

-- | Get the vertices of a polyline.
vertices :: Polyline dimension units space -> NonEmpty (Point dimension units space)
vertices (Polyline vertices_) = vertices_

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
length :: Point.Exists dimension units space => Polyline dimension units space -> Quantity units
length polyline = Quantity.sumOf Line.length (segments polyline)

map ::
  (Point dimension units1 space1 -> Point dimension units2 space2) ->
  Polyline dimension units1 space1 ->
  Polyline dimension units2 space2
map function polyline = Polyline (NonEmpty.map function (vertices polyline))
