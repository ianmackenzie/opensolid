module OpenSolid.Polygon2d
  ( Polygon2d
  , unsafe
  , fromVertices
  , inscribed
  , circumscribed
  , hexagon
  , signedArea
  , signedArea'
  , map
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Error qualified as Error
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Result qualified as Result
import OpenSolid.Triangle2d (Triangle2d (Triangle2d))
import OpenSolid.Triangle2d qualified as Triangle2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

-- | A polygon without holes defined by a list of vertices.
newtype Polygon2d vertex = Polygon2d (NonEmpty vertex)

instance FFI (Polygon2d (Point2d (space @ Meters))) where
  representation = FFI.classRepresentation "Polygon2d"

instance HasField "vertices" (Polygon2d vertex) (NonEmpty vertex) where
  getField (Polygon2d vs) = vs

data CoincidentVertices = CoincidentVertices deriving (Eq, Show, Error.Message)

unsafe :: NonEmpty vertex -> Polygon2d vertex
unsafe = Polygon2d

{-| Construct a polygon from its vertices.

The vertices must be given in order,
be distinct from each other (within the current tolerance),
and must form a single non-intersecting loop.
The last vertex may be equal to the first vertex, but does not need to be.
-}
fromVertices ::
  (Tolerance units, Vertex2d vertex (space @ units)) =>
  NonEmpty vertex ->
  Result CoincidentVertices (Polygon2d vertex)
fromVertices givenVertices = do
  let first = givenVertices.first
  case givenVertices.rest of
    [] -> Failure CoincidentVertices
    second : remaining
      | coincidentVertices first second -> Failure CoincidentVertices
      | otherwise -> Result.do
          deduplicatedRest <- deduplicateRest first second remaining
          let polygon = Polygon2d (first :| deduplicatedRest)
          Success polygon

deduplicateRest ::
  (Tolerance units, Vertex2d vertex (space @ units)) =>
  vertex ->
  vertex ->
  List vertex ->
  Result CoincidentVertices (List vertex)
deduplicateRest first last []
  | first == last = Success []
  | coincidentVertices first last = Failure CoincidentVertices
  | otherwise = Success [last]
deduplicateRest first current (next : remaining)
  | coincidentVertices current next = Failure CoincidentVertices
  | otherwise = Result.map (current :) (deduplicateRest first next remaining)

coincidentVertices :: (Tolerance units, Vertex2d vertex (space @ units)) => vertex -> vertex -> Bool
coincidentVertices first second = Vertex2d.position first ~= Vertex2d.position second

data EmptyPolygon = EmptyPolygon deriving (Eq, Show, Error.Message)

{-| Create a regular polygon with the given number of sides.

The polygon will be sized to fit within a circle with the given center point and diameter
(each polygon vertex will lie on the circle).
The polygon will be oriented such that its bottom-most edge is horizontal.
-}
inscribed ::
  Tolerance units =>
  Int ->
  ("centerPoint" ::: Point2d (space @ units), "diameter" ::: Qty units) ->
  Result EmptyPolygon (Polygon2d (Point2d (space @ units)))
inscribed numSides (Field centerPoint, Field diameter) =
  case Qty.midpoints (Angle.degrees -90.0) (Angle.degrees 270.0) numSides of
    [] -> Failure EmptyPolygon
    List.One{} -> Failure EmptyPolygon
    NonEmpty vertexAngles -> do
      let radius = 0.5 * diameter
      if radius ~= Qty.zero
        then Failure EmptyPolygon
        else do
          let vertex angle = centerPoint + Vector2d.polar radius angle
          let vertices = NonEmpty.map vertex vertexAngles
          Success (Polygon2d vertices)

{-| Create a regular polygon with the given number of sides.

The polygon will be sized so that
a circle with the given center point and diameter will just fit within the polygon
(each polygon edge will touch the circle at the edge's midpoint).
For a polygon with an even number of sides (square, hexagon, octagon etc.),
this means that the "width across flats" will be equal to the given circle diameter.
The polygon will be oriented such that its bottom-most edge is horizontal.
-}
circumscribed ::
  Tolerance units =>
  Int ->
  ("centerPoint" ::: Point2d (space @ units), "diameter" ::: Qty units) ->
  Result EmptyPolygon (Polygon2d (Point2d (space @ units)))
circumscribed numSides (Field centerPoint, Field diameter) = do
  let outerDiameter = diameter / Angle.cos (Angle.pi / Float.int numSides)
  inscribed numSides (#centerPoint centerPoint, #diameter outerDiameter)

{-| Create a hexagon with the given center point and height.

The hexagon will be oriented such that its top and bottom edges are horizontal.
-}
hexagon ::
  Tolerance units =>
  ("centerPoint" ::: Point2d (space @ units), "height" ::: Qty units) ->
  Result EmptyPolygon (Polygon2d (Point2d (space @ units)))
hexagon (Field centerPoint, Field height) =
  circumscribed 6 (#centerPoint centerPoint, #diameter height)

map :: (a -> b) -> Polygon2d a -> Polygon2d b
map function polygon = Polygon2d (NonEmpty.map function polygon.vertices)

signedArea' :: Vertex2d vertex (space @ units) => Polygon2d vertex -> Qty (units :*: units)
signedArea' (Polygon2d (v0 :| vs)) = do
  let triangleSignedArea' v1 v2 = Triangle2d.signedArea' (Triangle2d v0 v1 v2)
  Qty.sum (List.successive triangleSignedArea' vs)

signedArea ::
  (Vertex2d vertex (space @ units), Units.Squared units squaredUnits) =>
  Polygon2d vertex ->
  Qty squaredUnits
signedArea = Units.specialize . signedArea'

instance HasField "edges" (Polygon2d vertex) (NonEmpty (LineSegment2d vertex)) where
  getField (Polygon2d (v0 :| vs)) = collectEdges v0 v0 vs

collectEdges :: vertex -> vertex -> List vertex -> NonEmpty (LineSegment2d vertex)
collectEdges first current remaining = case remaining of
  [] -> NonEmpty.one (LineSegment2d current first)
  next : following -> NonEmpty.push (LineSegment2d current next) (collectEdges first next following)
