module Polyline2d
  ( Polyline2d (..)
  , map
  , start
  , end
  , length
  )
where

import NonEmpty qualified
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Point2d qualified as Point2d
import Vertex2d (Vertex2d)
import Vertex2d qualified

newtype Polyline2d vertex = Polyline2d {vertices :: NonEmpty vertex}

map :: (a -> b) -> Polyline2d a -> Polyline2d b
map function (Polyline2d vertices) = Polyline2d (NonEmpty.map function vertices)

start :: Polyline2d vertex -> vertex
start polyline = NonEmpty.first (vertices polyline)

end :: Polyline2d vertex -> vertex
end polyline = NonEmpty.last (vertices polyline)

length :: Vertex2d vertex (space @ units) => Polyline2d vertex -> Qty units
length polyline = Qty.sum (NonEmpty.successive segmentLength (vertices polyline))

segmentLength :: Vertex2d vertex (space @ units) => vertex -> vertex -> Qty units
segmentLength v1 v2 = Point2d.distanceFrom (Vertex2d.position v1) (Vertex2d.position v2)
