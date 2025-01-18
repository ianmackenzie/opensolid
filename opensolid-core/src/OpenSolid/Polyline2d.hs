module OpenSolid.Polyline2d
  ( Polyline2d (..)
  , map
  , start
  , end
  , length
  , segments
  )
where

import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.LineSegment2d qualified as LineSegment2d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Vertex2d (Vertex2d)

newtype Polyline2d vertex = Polyline2d {vertices :: NonEmpty vertex}

map :: (a -> b) -> Polyline2d a -> Polyline2d b
map function (Polyline2d vertices) = Polyline2d (NonEmpty.map function vertices)

start :: Polyline2d vertex -> vertex
start polyline = NonEmpty.first (vertices polyline)

end :: Polyline2d vertex -> vertex
end polyline = NonEmpty.last (vertices polyline)

segments :: Polyline2d vertex -> List (LineSegment2d vertex)
segments polyline = NonEmpty.successive LineSegment2d (vertices polyline)

length :: Vertex2d vertex (space @ units) => Polyline2d vertex -> Qty units
length polyline = Qty.sumOf LineSegment2d.length (segments polyline)
