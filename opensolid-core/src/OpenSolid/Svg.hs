module OpenSolid.Svg
  ( Svg
  , Attribute
  , Layout
  , viewBox
  , padding
  , toText
  , write
  , nothing
  , group
  , groupWith
  , combine
  , combineWith
  , line
  , lineWith
  , polyline
  , polylineWith
  , polygon
  , polygonWith
  , triangle
  , triangleWith
  , circle
  , circleWith
  , bounds
  , boundsWith
  , curve
  , curveWith
  , region
  , regionWith
  , arrow
  , arrowWith
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

import OpenSolid.Axis2D (Axis2D (Axis2D))
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
import OpenSolid.Polygon2D (Polygon2D (Polygon2D))
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
nodeText indent Node{tag, attributes, children} = do
  let attributeLines = List.map (attributeText ("\n" <> indent <> "   ")) attributes
  let openingTag = indent <> "<" <> tag <> Text.concat attributeLines <> ">"
  let childLines = List.map (nodeText (indent <> "  ")) children
  let closingTag = indent <> "</" <> tag <> ">"
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
toText layout entity = do
  let computedViewBox =
        case layout of
          ViewBox givenViewBox -> givenViewBox
          Padding givenPadding ->
            case entity of
              Svg Node{bounds = Bounds2D (Interval xLow xHigh) (Interval yLow yHigh)} ->
                Bounds2D
                  (Interval (xLow - givenPadding) (xHigh + givenPadding))
                  (Interval (yLow - givenPadding) (yHigh + givenPadding))
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
            Text.join " " $
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
          , children = List.maybe (toNode entity)
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
write path layout entity = IO.writeUtf8 path (toText layout entity)

{-| Don't draw anything at all.

Can be useful if you want to conditionally render some content.
-}
nothing :: Svg
nothing = Empty

-- | Group several SVG entities into a single entity, applying the given attributes to the group.
groupWith :: List Attribute -> List Svg -> Svg
groupWith attributes children =
  case List.filterMap toNode children of
    [] -> Empty
    NonEmpty childNodes ->
      Svg
        Node
          { bounds = Bounds2D.aggregateOf (.bounds) childNodes
          , tag = "g"
          , attributes
          , children = NonEmpty.toList childNodes
          }

-- | Group several SVG entities into a single entity.
group :: List Svg -> Svg
group = groupWith []

combine :: (a -> Svg) -> List a -> Svg
combine = combineWith []

combineWith :: List Attribute -> (a -> Svg) -> List a -> Svg
combineWith attributes function list =
  groupWith attributes (List.map function list)

-- | Draw a line with the given attributes.
lineWith :: List Attribute -> Line2D Meters -> Svg
lineWith attributes (Line2D p1 p2) = do
  let Point2D x1 y1 = p1
  let Point2D x2 y2 = p2
  let x1Attribute = Attribute "x1" (lengthText x1)
  let y1Attribute = Attribute "y1" (lengthText -y1)
  let x2Attribute = Attribute "x2" (lengthText x2)
  let y2Attribute = Attribute "y2" (lengthText -y2)
  Svg
    Node
      { bounds = Bounds2D.hull2 p1 p2
      , tag = "line"
      , attributes = x1Attribute : y1Attribute : x2Attribute : y2Attribute : attributes
      , children = []
      }

-- | Draw a line.
line :: Line2D Meters -> Svg
line = lineWith []

-- | Draw a polyline with the given attributes.
polylineWith :: List Attribute -> Polyline2D Meters -> Svg
polylineWith attributes givenPolyline = do
  let points = NonEmpty.toList (Polyline2D.vertices givenPolyline)
  Svg
    Node
      { bounds = Polyline2D.bounds givenPolyline
      , tag = "polyline"
      , attributes = noFill : pointsAttribute points : attributes
      , children = []
      }

-- | Draw a polyline.
polyline :: Polyline2D Meters -> Svg
polyline = polylineWith []

-- | Draw a polygon with the given attributes.
polygonWith :: List Attribute -> Polygon2D Meters -> Svg
polygonWith attributes givenPolygon = do
  let points = NonEmpty.toList (Polygon2D.vertices givenPolygon)
  Svg
    Node
      { bounds = Polygon2D.bounds givenPolygon
      , tag = "polygon"
      , attributes = pointsAttribute points : attributes
      , children = []
      }

-- | Draw a polygon.
polygon :: Polygon2D Meters -> Svg
polygon = polygonWith []

-- | Draw a triangle with the given attributes.
triangleWith :: List Attribute -> Triangle2D Meters -> Svg
triangleWith attributes (Triangle2D p1 p2 p3) = do
  let vertices = NonEmpty.three p1 p2 p3
  polygonWith attributes (Polygon2D vertices)

-- | Draw a triangle.
triangle :: Triangle2D Meters -> Svg
triangle = triangleWith []

bounds :: Bounds2D Meters -> Svg
bounds = boundsWith []

boundsWith :: List Attribute -> Bounds2D Meters -> Svg
boundsWith attributes givenBounds = do
  let Bounds2D xInterval yInterval = givenBounds
  let xAttribute = Attribute "x" (lengthText (Interval.lower xInterval))
  let yAttribute = Attribute "y" (lengthText (negate (Interval.upper yInterval)))
  let widthAttribute = Attribute "width" (lengthText (Interval.width xInterval))
  let heightAttribute = Attribute "height" (lengthText (Interval.width yInterval))
  Svg
    Node
      { bounds = givenBounds
      , tag = "rect"
      , attributes = xAttribute : yAttribute : widthAttribute : heightAttribute : attributes
      , children = []
      }

-- | Draw a circle with the given attributes.
circleWith :: List Attribute -> Circle2D Meters -> Svg
circleWith attributes givenCircle = do
  let Point2D cx cy = Circle2D.centerPoint givenCircle
  let cxAttribute = Attribute "cx" (lengthText cx)
  let cyAttribute = Attribute "cy" (lengthText -cy)
  let rAttribute = Attribute "r" (lengthText (Circle2D.radius givenCircle))
  Svg
    Node
      { bounds = Circle2D.bounds givenCircle
      , tag = "circle"
      , attributes = cxAttribute : cyAttribute : rAttribute : attributes
      , children = []
      }

-- | Draw a circle.
circle :: Circle2D Meters -> Svg
circle = circleWith []

-- | Draw a curve with the given attributes and resolution.
curveWith :: List Attribute -> Resolution Meters -> Curve2D Meters -> Svg
curveWith attributes resolution givenCurve =
  polylineWith attributes (Curve2D.toPolyline resolution givenCurve)

-- | Draw a curve with the given resolution.
curve :: Resolution Meters -> Curve2D Meters -> Svg
curve = curveWith []

region :: Resolution Meters -> Region2D Meters -> Svg
region = regionWith []

regionWith :: List Attribute -> Resolution Meters -> Region2D Meters -> Svg
regionWith attributes resolution givenRegion = do
  let outerPolygon = loopPolygon resolution (Region2D.outerLoop givenRegion)
  let innerPolygons = List.map (loopPolygon resolution) (Region2D.innerLoops givenRegion)
  let allPolygons = outerPolygon : innerPolygons
  let dAttribute = Attribute "d" (Text.join " " (List.map polygonCommands allPolygons))
  Svg
    Node
      { bounds = Polygon2D.bounds outerPolygon
      , tag = "path"
      , attributes = dAttribute : attributes
      , children = []
      }

loopPolygon :: Resolution Meters -> NonEmpty (Curve2D Meters) -> Polygon2D Meters
loopPolygon resolution loop = do
  let curvePoints = Curve2D.toPolyline resolution >> Polyline2D.vertices >> NonEmpty.rest
  Polygon2D $
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
  "start" ::: Point2D Meters ->
  "end" ::: Point2D Meters ->
  "headLength" ::: Length ->
  "headWidth" ::: Length ->
  Svg
arrow = arrowWith []

arrowWith ::
  List Attribute ->
  "start" ::: Point2D Meters ->
  "end" ::: Point2D Meters ->
  "headLength" ::: Length ->
  "headWidth" ::: Length ->
  Svg
arrowWith
  attributes
  ("start" ::: start)
  ("end" ::: end)
  ("headLength" ::: headLength)
  ("headWidth" ::: headWidth) =
    case Tolerance.using Quantity.zero (Direction2D.from start end) of
      Error Direction2D.PointsAreCoincident -> nothing
      Ok direction -> do
        let length = Point2D.distanceFrom start end
        let axis = Axis2D start direction
        let frame = Frame2D.fromXAxis axis
        let stemLength = length - headLength
        let stemEndPoint = Point2D.along axis stemLength
        let leftPoint = Point2D.placeIn frame (Point2D stemLength (0.5 * headWidth))
        let rightPoint = Point2D.mirrorAcross axis leftPoint
        let stem = line (Line2D start stemEndPoint)
        let tip = triangle (Triangle2D leftPoint rightPoint end)
        groupWith attributes [stem, tip]

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
