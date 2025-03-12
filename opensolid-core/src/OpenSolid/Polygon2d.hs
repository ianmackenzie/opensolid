module OpenSolid.Polygon2d
  ( Polygon2d (Polygon2d)
  , vertices
  , edges
  , signedArea
  , signedArea'
  , map
  )
where

import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Triangle2d (Triangle2d (Triangle2d))
import OpenSolid.Triangle2d qualified as Triangle2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vertex2d (Vertex2d)

newtype Polygon2d vertex = Polygon2d {vertices :: NonEmpty vertex}

map :: (a -> b) -> Polygon2d a -> Polygon2d b
map function (Polygon2d vertices) = Polygon2d (NonEmpty.map function vertices)

signedArea' :: Vertex2d vertex (space @ units) => Polygon2d vertex -> Qty (units :*: units)
signedArea' (Polygon2d (v0 :| vs)) = do
  let triangleSignedArea' v1 v2 = Triangle2d.signedArea' (Triangle2d v0 v1 v2)
  Qty.sum (List.successive triangleSignedArea' vs)

signedArea ::
  (Vertex2d vertex (space @ units), Units.Squared units squaredUnits) =>
  Polygon2d vertex ->
  Qty squaredUnits
signedArea = Units.specialize . signedArea'

edges :: Polygon2d vertex -> NonEmpty (LineSegment2d vertex)
edges (Polygon2d (v0 :| vs)) = collectEdges v0 v0 vs

collectEdges :: vertex -> vertex -> List vertex -> NonEmpty (LineSegment2d vertex)
collectEdges first current remaining = case remaining of
  [] -> NonEmpty.one (LineSegment2d current first)
  next : following ->
    NonEmpty.prepend (LineSegment2d current next) (collectEdges first next following)
