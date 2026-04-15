module OpenSolid.Svg
  ( Svg
  , Attribute
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
import OpenSolid.Line2D (Line2D, pattern Line2D)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D, pattern Point2D)
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
import OpenSolid.Set2D qualified as Set2D
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Triangle2D (Triangle2D (Triangle2D))

-- | Some SVG drawing content such as a shape with attributes.
data Svg = Empty | Node Text (List Attribute) (List Svg)

-- | An SVG attribute such as fill color or stroke width.
data Attribute = Attribute Text Text deriving (Show)

instance FFI Svg where
  representation = FFI.classRepresentation "Svg"

instance FFI Attribute where
  representation = FFI.nestedClassRepresentation "Svg" "Attribute"

svgText :: Text -> Svg -> Maybe Text
svgText _ Empty = Nothing
svgText indent (Node name attributes children) = Just (nodeText indent name attributes children)

nodeText :: Text -> Text -> List Attribute -> List Svg -> Text
nodeText indent name attributes children = do
  let attributeLines = List.map (attributeText ("\n" <> indent <> "   ")) attributes
  let openingTag = indent <> "<" <> name <> Text.concat attributeLines <> ">"
  let childLines = List.filterMap (svgText (indent <> "  ")) children
  let closingTag = indent <> "</" <> name <> ">"
  Text.multiline (openingTag : childLines) <> "\n" <> closingTag

attributeText :: Text -> Attribute -> Text
attributeText indent (Attribute name value) = indent <> name <> "=\"" <> value <> "\""

{-| Convert an SVG drawing to text.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.

In most cases it's more convenient to write a file directly using 'write',
but 'toText' can be useful if you want to e.g. return the SVG text as part of an HTTP response.
-}
toText :: Bounds2D Meters -> Svg -> Text
toText viewBox entity = do
  let Bounds2D xBounds yBounds = viewBox
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
  Text.multiline
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    , nodeText "" "svg" attributes [entity]
    , "" -- Ensure file has trailing newline
    ]

{-| Write an SVG drawing to a file.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.
-}
write :: Text -> Bounds2D Meters -> Svg -> IO ()
write path viewBox entity = IO.writeUtf8 path (toText viewBox entity)

{-| Don't draw anything at all.

Can be useful if you want to conditionally render some content.
-}
nothing :: Svg
nothing = Empty

-- | Group several SVG entities into a single entity, applying the given attributes to the group.
groupWith :: List Attribute -> List Svg -> Svg
groupWith = Node "g"

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
  Node "line" (x1Attribute : y1Attribute : x2Attribute : y2Attribute : attributes) []

-- | Draw a line.
line :: Line2D Meters -> Svg
line = lineWith []

-- | Draw a polyline with the given attributes.
polylineWith :: List Attribute -> Polyline2D Meters -> Svg
polylineWith attributes givenPolyline = do
  let points = NonEmpty.toList (Polyline2D.vertices givenPolyline)
  Node "polyline" (noFill : pointsAttribute points : attributes) []

-- | Draw a polyline.
polyline :: Polyline2D Meters -> Svg
polyline = polylineWith []

-- | Draw a polygon with the given attributes.
polygonWith :: List Attribute -> Polygon2D Meters -> Svg
polygonWith attributes givenPolygon = do
  let points = NonEmpty.toList (Polygon2D.vertices givenPolygon)
  Node "polygon" (pointsAttribute points : attributes) []

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
boundsWith attributes (Bounds2D xInterval yInterval) = do
  let xAttribute = Attribute "x" (lengthText (Interval.lower xInterval))
  let yAttribute = Attribute "y" (lengthText (negate (Interval.upper yInterval)))
  let widthAttribute = Attribute "width" (lengthText (Interval.width xInterval))
  let heightAttribute = Attribute "height" (lengthText (Interval.width yInterval))
  Node "rect" (xAttribute : yAttribute : widthAttribute : heightAttribute : attributes) []

-- | Draw a circle with the given attributes, center point and diameter.
circleWith :: List Attribute -> "centerPoint" ::: Point2D Meters -> "diameter" ::: Length -> Svg
circleWith attributes ("centerPoint" ::: centerPoint) ("diameter" ::: diameter) = do
  let Point2D cx cy = centerPoint
  let cxAttribute = Attribute "cx" (lengthText cx)
  let cyAttribute = Attribute "cy" (lengthText -cy)
  let rAttribute = Attribute "r" (lengthText (0.5 * diameter))
  Node "circle" (cxAttribute : cyAttribute : rAttribute : attributes) []

-- | Draw a circle with the given center point and diameter.
circle :: "centerPoint" ::: Point2D Meters -> "diameter" ::: Length -> Svg
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
  let loops = Region2D.boundaryLoops givenRegion & NonEmpty.toList & List.map Set2D.toNonEmpty
  let dAttribute = Attribute "d" (Text.join " " (List.map (loopCommands resolution) loops))
  Node "path" (dAttribute : attributes) []

loopCommands :: Resolution Meters -> NonEmpty (Curve2D Meters) -> Text
loopCommands resolution curves = do
  let startPoint = Curve2D.startPoint (NonEmpty.first curves)
  let numCurves = NonEmpty.length curves
  let drawCurve curveIndex loopCurve = do
        let points =
              Curve2D.toPolyline resolution loopCurve
                & Polyline2D.vertices
                & NonEmpty.rest -- drop the first point, we're assumed to be there already
        let numPoints = List.length points
        let isLastCurve = curveIndex == numCurves - 1
        let pointCommand pointIndex curvePoint = do
              let isLastPoint = pointIndex == numPoints - 1
              if isLastCurve && isLastPoint then "Z" else lineTo curvePoint
        let pointCommands = List.mapWithIndex pointCommand points
        Text.join " " pointCommands
  Text.join " " (moveTo startPoint : List.mapWithIndex drawCurve (NonEmpty.toList curves))

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
