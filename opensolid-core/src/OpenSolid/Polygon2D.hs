module OpenSolid.Polygon2D
  ( Polygon2D (Polygon2D)
  , inscribed
  , circumscribed
  , hexagon
  , vertices
  , edges
  , signedArea
  , signedArea_
  , map
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Line2D (Line2D (Line2D))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Triangle2D (Triangle2D (Triangle2D))
import OpenSolid.Triangle2D qualified as Triangle2D
import OpenSolid.Units qualified as Units

{-| A non-empty list of points joined by lines.

The last point will be joined back to the first.
-}
newtype Polygon2D units space
  = -- | Construct a polygon from its boundary vertices.
    --
    -- These should generally be in counterclockwise order
    -- (clockwise order will be interpreted as a 'negative' polygon or hole).
    Polygon2D {vertices :: NonEmpty (Point2D units space)}

instance FFI (Polygon2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Polygon2D"

instance FFI (Polygon2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvPolygon"

data IsEmpty = IsEmpty deriving (Eq, Show)

{-| Create a regular polygon with the given number of vertices/sides.

The polygon will be sized to fit within the given circle
(each polygon vertex will lie on the circle).
The polygon will be oriented such that its bottom-most edge is horizontal.
-}
inscribed :: Int -> Circle2D units space -> Result IsEmpty (Polygon2D units space)
inscribed n circle
  | n >= 3 = case Quantity.midpoints (Angle.degrees -90) (Angle.degrees 270) n of
      NonEmpty vertexAngles -> Ok (Polygon2D (NonEmpty.map (Circle2D.pointOn circle) vertexAngles))
      [] -> throw (InternalError "Should have at least three vertex angles")
  | otherwise = Error IsEmpty

{-| Create a regular polygon with the given number of vertices/sides.

The polygon will be sized so that the given circle will just fit within the polygon
(each polygon edge will touch the circle at the edge's midpoint).
For a polygon with an even number of sides (square, hexagon, octagon etc.),
this means that the "height" or "width across flats" will be equal to the given circle's diameter.
The polygon will be oriented such that its bottom-most edge is horizontal.
-}
circumscribed :: Int -> Circle2D units space -> Result IsEmpty (Polygon2D units space)
circumscribed n circle = do
  let outerDiameter = Circle2D.diameter circle ./. Angle.cos (Angle.pi ./. Number.fromInt n)
  let outerCircle = Circle2D.withDiameter outerDiameter (Circle2D.centerPoint circle)
  inscribed n outerCircle

{-| Create a hexagon with the given center point and height.

The hexagon will be oriented such that its top and bottom edges are horizontal.
-}
hexagon ::
  "centerPoint" ::: Point2D units space ->
  "height" ::: Quantity units ->
  Result IsEmpty (Polygon2D units space)
hexagon (Named centerPoint) (Named height) =
  circumscribed 6 (Circle2D.withDiameter height centerPoint)

-- | Get the vertices of a polygon.
vertices :: Polygon2D units space -> NonEmpty (Point2D units space)
vertices = (.vertices)

-- | Get the edges of a polygon.
edges :: Polygon2D units space -> NonEmpty (Line2D units space)
edges polygon = do
  let v0 :| vs = vertices polygon
  collectEdges v0 v0 vs

signedArea_ :: Polygon2D units space -> Quantity (units ?*? units)
signedArea_ polygon = do
  let v0 :| vs = vertices polygon
  let triangleSignedArea_ v1 v2 = Triangle2D.signedArea_ (Triangle2D v0 v1 v2)
  Quantity.sum (List.successive triangleSignedArea_ vs)

{-| Get the signed area of a polygon.

This will be positive if the polygon has counterclockwise orientation,
and negative otherwise.
-}
signedArea :: Units.Squared units1 units2 => Polygon2D units1 space -> Quantity units2
signedArea = Units.specialize . signedArea_

collectEdges ::
  Point2D units space ->
  Point2D units space ->
  List (Point2D units space) ->
  NonEmpty (Line2D units space)
collectEdges first current remaining = case remaining of
  [] -> NonEmpty.one (Line2D current first)
  next : following -> NonEmpty.push (Line2D current next) (collectEdges first next following)

map ::
  (Point2D units1 space1 -> Point2D units2 space2) ->
  Polygon2D units1 space1 ->
  Polygon2D units2 space2
map function polyline = Polygon2D (NonEmpty.map function (vertices polyline))
