module OpenSolid.Polygon2d
  ( Polygon2d (Polygon2d)
  , vertices
  , edges
  , signedArea
  , signedArea_
  , map
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Polymorphic.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Triangle2d (Triangle2d (Triangle2d))
import OpenSolid.Triangle2d qualified as Triangle2d
import OpenSolid.Units qualified as Units

{-| A non-empty list of points joined by line segments.

The last point will be joined back to the first.
-}
newtype Polygon2d units space
  = -- | Construct a polygon from its boundary vertices.
    --
    -- These should generally be in counterclockwise order
    -- (clockwise order will be interpreted as a 'negative' polygon or hole).
    Polygon2d {vertices :: NonEmpty (Point2d units space)}

instance FFI (Polygon2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Polygon2d"

-- | Get the vertices of a polygon.
vertices :: Polygon2d units space -> NonEmpty (Point2d units space)
vertices = (.vertices)

-- | Get the edges of a polygon.
edges :: Polygon2d units space -> NonEmpty (LineSegment2d units space)
edges polygon = do
  let v0 :| vs = vertices polygon
  collectEdges v0 v0 vs

signedArea_ :: Polygon2d units space -> Quantity (units ?*? units)
signedArea_ polygon = do
  let v0 :| vs = vertices polygon
  let triangleSignedArea_ v1 v2 = Triangle2d.signedArea_ (Triangle2d v0 v1 v2)
  Quantity.sum (List.successive triangleSignedArea_ vs)

{-| Get the signed area of a polygon.

This will be positive if the polygon has counterclockwise orientation,
and negative otherwise.
-}
signedArea :: Units.Squared units1 units2 => Polygon2d units1 space -> Quantity units2
signedArea = Units.specialize . signedArea_

collectEdges ::
  Point2d units space ->
  Point2d units space ->
  List (Point2d units space) ->
  NonEmpty (LineSegment2d units space)
collectEdges first current remaining = case remaining of
  [] -> NonEmpty.one (LineSegment2d current first)
  next : following -> NonEmpty.push (LineSegment2d current next) (collectEdges first next following)

map ::
  (Point2d units1 space1 -> Point2d units2 space2) ->
  Polygon2d units1 space1 ->
  Polygon2d units2 space2
map function polyline = Polygon2d (NonEmpty.map function (vertices polyline))
