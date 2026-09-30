module OpenSolid.Svg
  ( Svg
  , Attribute
  , Layout
  , viewBox
  , padding
  , toText
  , write
  , nothing
  , with
  , group
  , combine
  , line
  , lineFrom
  , polyline
  , polygon
  , triangle
  , circle
  , circleWith
  , bounds
  , curve
  , region
  , arrow
  , blackStroke
  , strokeColor
  , strokeWidth
  , noFill
  , whiteFill
  , fillColor
  , opacity
  , miterStrokeJoins
  , roundStrokeJoins
  , bevelStrokeJoins
  , noStrokeCaps
  , roundStrokeCaps
  , squareStrokeCaps
  )
where

import Data.Foldable qualified
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.IO qualified as IO
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2D (Line2D, data Line2D)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D, data Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Polygon2D (Polygon2D)
import OpenSolid.Polygon2D qualified as Polygon2D
import OpenSolid.Polyline2D (Polyline2D)
import OpenSolid.Polyline2D qualified as Polyline2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution (Resolution)
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Triangle2D (Triangle2D (Triangle2D))

-- | Some SVG drawing content such as a shape with attributes.
data Svg = Svg Node | Empty

data Node = Node
  { bounds :: Bounds2D Meters
  , tag :: Text
  , attributes :: List Attribute
  , children :: List Node
  }

toNode :: Svg -> Maybe Node
toNode (Svg node) = Just node
toNode Empty = Nothing

-- | An SVG attribute such as fill color or stroke width.
data Attribute = Attribute Text Text deriving (Show)

-- | The definition of the overall size and shape of an SVG drawing.
data Layout = ViewBox (Bounds2D Meters) | Padding Length

-- | Specify the exact view box that should be used for the drawing.
viewBox :: Bounds2D Meters -> Layout
viewBox = ViewBox

{-| Specify a padding that should be used around drawing entities.

The overall view box will be determined by adding this padding
to the overall bounding box around all drawing entities.
-}
padding :: Length -> Layout
padding = Padding

instance FFI Svg where
  representation = FFI.classRepresentation "Svg"

instance FFI Attribute where
  representation = FFI.nestedClassRepresentation "Svg" "Attribute"

instance FFI Layout where
  representation = FFI.nestedClassRepresentation "Svg" "Layout"

nodeText :: Text -> Node -> Text
nodeText indent node = do
  let attributeLines = List.map (attributeText ("\n" <> indent <> "   ")) node.attributes
  let openingTag = indent <> "<" <> node.tag <> Text.concat attributeLines <> ">"
  let childLines = List.map (nodeText (indent <> "  ")) node.children
  let closingTag = indent <> "</" <> node.tag <> ">"
  Text.multiline (openingTag : childLines) <> "\n" <> closingTag

attributeText :: Text -> Attribute -> Text
attributeText indent (Attribute name value) = indent <> name <> "=\"" <> value <> "\""

{-| Convert an SVG drawing to text.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.

In most cases it's more convenient to write a file directly using 'write',
but 'toText' can be useful if you want to e.g. return the SVG text as part of an HTTP response.
-}
toText :: Layout -> Svg -> Text
toText layout svg = do
  let computedViewBox =
        case layout of
          ViewBox givenViewBox -> givenViewBox
          Padding givenPadding ->
            case svg of
              Svg node -> do
                let Bounds2D (Interval xLow xHigh) (Interval yLow yHigh) = node.bounds
                let paddedX = Interval (xLow - givenPadding) (xHigh + givenPadding)
                let paddedY = Interval (yLow - givenPadding) (yHigh + givenPadding)
                Bounds2D paddedX paddedY
              Empty -> Bounds2D.constant Point2D.origin
  let Bounds2D xBounds yBounds = computedViewBox
  let Interval x1 x2 = xBounds
  let Interval y1 y2 = yBounds
  let width = x2 - x1
  let height = y2 - y1
  let attributes =
        [ Attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attribute "version" "1.1"
        , Attribute "width" (lengthText width <> "mm")
        , Attribute "height" (lengthText height <> "mm")
        , Attribute "viewBox" $
            Text.sentence
              [ lengthText x1
              , lengthText -y2
              , lengthText width
              , lengthText height
              ]
        , blackStroke
        , strokeWidth (Length.pixels 1.0)
        , noFill
        ]
  let svgNode =
        Node
          { bounds = computedViewBox
          , tag = "svg"
          , attributes
          , children = List.maybe (toNode svg)
          }
  Text.multiline
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    , nodeText "" svgNode
    , "" -- Ensure file has trailing newline
    ]

{-| Write an SVG drawing to a file.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.
-}
write :: Text -> Layout -> Svg -> IO ()
write path layout svg = IO.writeUtf8 path (toText layout svg)

{-| Don't draw anything at all.

Can be useful if you want to conditionally render some content.
-}
nothing :: Svg
nothing = Empty

entity :: Text -> Bounds2D Meters -> List Attribute -> Svg
entity givenTag givenBounds givenAttributes =
  Svg
    Node
      { bounds = givenBounds
      , tag = givenTag
      , attributes = givenAttributes
      , children = []
      }

-- | Add the given attributes to an SVG entity.
with :: List Attribute -> Svg -> Svg
with givenAttributes svg = case svg of
  Empty -> Empty
  Svg node ->
    Svg
      Node
        { bounds = node.bounds
        , tag = node.tag
        , attributes = givenAttributes <> node.attributes
        , children = node.children
        }

-- | Group several SVG entities into a single entity.
group :: List Svg -> Svg
group children =
  case List.filterMap toNode children of
    [] -> Empty
    NonEmpty childNodes ->
      Svg
        Node
          { bounds = Bounds2D.aggregateOf (.bounds) childNodes
          , tag = "g"
          , attributes = []
          , children = NonEmpty.toList childNodes
          }

combine :: Foldable list => (a -> Svg) -> list a -> Svg
combine function list = group (List.map function (Data.Foldable.toList list))

-- | Draw a line.
line :: Line2D Meters -> Svg
line (Line2D p1 p2) = do
  let Point2D x1 y1 = p1
  let Point2D x2 y2 = p2
  let x1Attribute = Attribute "x1" (lengthText x1)
  let y1Attribute = Attribute "y1" (lengthText -y1)
  let x2Attribute = Attribute "x2" (lengthText x2)
  let y2Attribute = Attribute "y2" (lengthText -y2)
  entity "line" (Bounds2D.hull2 p1 p2) [x1Attribute, y1Attribute, x2Attribute, y2Attribute]

-- | Draw a line between two points.
lineFrom :: Point2D Meters -> Point2D Meters -> Svg
lineFrom p1 p2 = line (Line2D p1 p2)

-- | Draw a polyline.
polyline :: Polyline2D Meters -> Svg
polyline givenPolyline = do
  let points = NonEmpty.toList (Polyline2D.vertices givenPolyline)
  entity "polyline" (Polyline2D.bounds givenPolyline) [pointsAttribute points, noFill]

-- | Draw a polygon.
polygon :: Polygon2D Meters -> Svg
polygon givenPolygon = do
  let points = NonEmpty.toList (Polygon2D.vertices givenPolygon)
  entity "polygon" (Polygon2D.bounds givenPolygon) [pointsAttribute points]

-- | Draw a triangle.
triangle :: Triangle2D Meters -> Svg
triangle (Triangle2D p1 p2 p3) = do
  let vertices = NonEmpty.three p1 p2 p3
  polygon (Polygon2D.fromVertices vertices)

-- | Draw a bounding box.
bounds :: Bounds2D Meters -> Svg
bounds givenBounds = do
  let Bounds2D xInterval yInterval = givenBounds
  let xAttribute = Attribute "x" (lengthText (Interval.lower xInterval))
  let yAttribute = Attribute "y" (lengthText (negate (Interval.upper yInterval)))
  let widthAttribute = Attribute "width" (lengthText (Interval.width xInterval))
  let heightAttribute = Attribute "height" (lengthText (Interval.width yInterval))
  entity "rect" givenBounds [xAttribute, yAttribute, widthAttribute, heightAttribute]

-- | Draw a circle.
circle :: Circle2D Meters -> Svg
circle givenCircle = do
  let Point2D cx cy = Circle2D.centerPoint givenCircle
  let cxAttribute = Attribute "cx" (lengthText cx)
  let cyAttribute = Attribute "cy" (lengthText -cy)
  let rAttribute = Attribute "r" (lengthText (Circle2D.radius givenCircle))
  entity "circle" (Circle2D.bounds givenCircle) [cxAttribute, cyAttribute, rAttribute]

-- | Draw a circle with the given center point and diameter.
circleWith :: "centerPoint" ::: Point2D Meters -> "diameter" ::: Length -> Svg
circleWith ("centerPoint" ::: centerPoint) ("diameter" ::: diameter) =
  circle (Circle2D.with {centerPoint, diameter})

-- | Draw a curve with the given resolution.
curve :: Resolution Meters -> Curve2D Meters -> Svg
curve resolution = polyline . Curve2D.toPolyline resolution

region :: Resolution Meters -> Region2D Meters -> Svg
region resolution givenRegion = do
  let outerPolygon = loopPolygon resolution (Region2D.outerLoop givenRegion)
  let innerPolygons = List.map (loopPolygon resolution) (Region2D.innerLoops givenRegion)
  let allPolygons = outerPolygon : innerPolygons
  let pathAttribute = Attribute "d" (Text.join " " (List.map polygonCommands allPolygons))
  entity "path" (Polygon2D.bounds outerPolygon) [pathAttribute]

loopPolygon :: Resolution Meters -> NonEmpty (Curve2D Meters) -> Polygon2D Meters
loopPolygon resolution loop = do
  let curvePoints = NonEmpty.rest . Polyline2D.vertices . Curve2D.toPolyline resolution
  Polygon2D.fromVertices $
    case List.combine curvePoints loop of
      NonEmpty vertices -> vertices
      [] -> NonEmpty.one (Curve2D.startPoint (NonEmpty.first loop)) -- Shouldn't happen for a valid region

polygonCommands :: Polygon2D Meters -> Text
polygonCommands givenPolygon = do
  let first :| rest = Polygon2D.vertices givenPolygon
  Text.join " " [moveTo first, Text.join " " (List.map lineTo rest), "Z"]

moveTo :: Point2D Meters -> Text
moveTo (Point2D x y) = Text.join " " ["M", lengthText x, lengthText -y]

lineTo :: Point2D Meters -> Text
lineTo (Point2D x y) = Text.join " " ["L", lengthText x, lengthText -y]

arrow ::
  "headLength" ::: Length ->
  "headWidth" ::: Length ->
  Point2D Meters ->
  Point2D Meters ->
  Svg
arrow ("headLength" ::: headLength) ("headWidth" ::: headWidth) startPoint endPoint =
  case Tolerance.using Quantity.zero (Direction2D.from startPoint endPoint) of
    Err Direction2D.PointsAreCoincident -> nothing
    Ok direction -> do
      let length = Point2D.distanceFrom startPoint endPoint
      let axis = Axis2D.with {originPoint = startPoint, direction}
      let frame = Frame2D.fromXAxis axis
      let stemLength = length - headLength
      let stemEndPoint = on axis stemLength
      let leftPoint = Point2D.placeIn frame (Point2D stemLength (0.5 * headWidth))
      let rightPoint = Point2D.mirrorAcross axis leftPoint
      let stem = line (Line2D startPoint stemEndPoint)
      let head = triangle (Triangle2D leftPoint rightPoint endPoint)
      group [stem, head]

pointsAttribute :: List (Point2D Meters) -> Attribute
pointsAttribute givenPoints =
  Attribute "points" (Text.join " " (List.map coordinatesText givenPoints))

coordinatesText :: Point2D Meters -> Text
coordinatesText (Point2D x y) = lengthText x <> "," <> lengthText -y

lengthText :: Length -> Text
lengthText givenLength = Text.number (Length.inMillimeters givenLength)

-- | Black stroke for curves and borders.
blackStroke :: Attribute
blackStroke = Attribute "stroke" "black"

-- | Set the stroke color for curves and borders.
strokeColor :: Color -> Attribute
strokeColor color = Attribute "stroke" (Color.toHex color)

strokeWidth :: Length -> Attribute
strokeWidth givenWidth = Attribute "stroke-width" (lengthText givenWidth)

-- | Set shapes to have no fill.
noFill :: Attribute
noFill = Attribute "fill" "none"

-- | Set shapes to have white fill.
whiteFill :: Attribute
whiteFill = Attribute "fill" "white"

-- | Set the fill color for shapes.
fillColor :: Color -> Attribute
fillColor color = Attribute "fill" (Color.toHex color)

opacity :: Number -> Attribute
opacity value = Attribute "opacity" (Text.number value)

miterStrokeJoins :: Attribute
miterStrokeJoins = Attribute "stroke-linejoin" "miter"

roundStrokeJoins :: Attribute
roundStrokeJoins = Attribute "stroke-linejoin" "round"

bevelStrokeJoins :: Attribute
bevelStrokeJoins = Attribute "stroke-linejoin" "bevel"

noStrokeCaps :: Attribute
noStrokeCaps = Attribute "stroke-linecap" "butt"

roundStrokeCaps :: Attribute
roundStrokeCaps = Attribute "stroke-linecap" "round"

squareStrokeCaps :: Attribute
squareStrokeCaps = Attribute "stroke-linecap" "square"
