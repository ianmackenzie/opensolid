module OpenSolid.Polyline2d
  ( Polyline2d (Polyline2d, vertices)
  , numVertices
  , start
  , end
  , segments
  , length
  , map
  )
where

import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.LineSegment2d qualified as LineSegment2d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vertex2d (Vertex2d)

newtype Polyline2d vertex = Polyline2d {vertices :: NonEmpty vertex}

map :: (a -> b) -> Polyline2d a -> Polyline2d b
map function (Polyline2d vertices) = Polyline2d (NonEmpty.map function vertices)

numVertices :: Polyline2d vertex -> Int
numVertices polyline = NonEmpty.length polyline.vertices

start :: Polyline2d vertex -> vertex
start polyline = NonEmpty.first polyline.vertices

end :: Polyline2d vertex -> vertex
end polyline = NonEmpty.last polyline.vertices

segments :: Polyline2d vertex -> List (LineSegment2d vertex)
segments polyline = NonEmpty.successive LineSegment2d polyline.vertices

length :: Vertex2d vertex (space @ units) => Polyline2d vertex -> Quantity units
length polyline = Quantity.sumOf LineSegment2d.length (segments polyline)
