module OpenSolid.Polyline2d
  ( Polyline2d (Polyline2d, vertices)
  , map
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

instance HasField "numVertices" (Polyline2d vertex) Int where
  getField = (.vertices.length)

instance HasField "start" (Polyline2d vertex) vertex where
  getField = (.vertices.first)

instance HasField "end" (Polyline2d vertex) vertex where
  getField = (.vertices.last)

instance HasField "segments" (Polyline2d vertex) (List (LineSegment2d vertex)) where
  getField polyline = NonEmpty.successive LineSegment2d polyline.vertices

instance Vertex2d vertex (space @ units) => HasField "length" (Polyline2d vertex) (Qty units) where
  getField polyline = Qty.sumOf LineSegment2d.length polyline.segments
