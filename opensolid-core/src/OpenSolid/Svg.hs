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
  , curve
  , curveWith
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
import OpenSolid.Interval (Interval (Interval))
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
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2D (Line2D (Line2D))
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
import OpenSolid.Resolution (Resolution)
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Triangle2D (Triangle2D (Triangle2D))

-- | Some SVG drawing content such as a shape with attributes.
data Svg space = Empty | Node Text (List (Attribute space)) (List (Svg space))

-- | An SVG attribute such as fill color or stroke width.
data Attribute space = Attribute Text Text deriving (Show)

instance FFI (Svg FFI.Space) where
  representation = FFI.classRepresentation "Svg"

instance FFI (Attribute FFI.Space) where
  representation = FFI.nestedClassRepresentation "Svg" "Attribute"

svgText :: Text -> Svg space -> Maybe Text
svgText _ Empty = Nothing
svgText indent (Node name attributes children) = Just (nodeText indent name attributes children)

nodeText :: Text -> Text -> List (Attribute space) -> List (Svg space) -> Text
nodeText indent name attributes children = do
  let attributeLines = List.map (attributeText ("\n" <> indent <> "   ")) attributes
  let openingTag = indent <> "<" <> name <> Text.concat attributeLines <> ">"
  let childLines = List.filterMap (svgText (indent <> "  ")) children
  let closingTag = indent <> "</" <> name <> ">"
  Text.multiline (openingTag : childLines) <> "\n" <> closingTag

attributeText :: Text -> Attribute space -> Text
attributeText indent (Attribute name value) = indent <> name <> "=\"" <> value <> "\""

{-| Convert an SVG drawing to text.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.

In most cases it's more convenient to write a file directly using 'write',
but 'toText' can be useful if you want to e.g. return the SVG text as part of an HTTP response.
-}
toText :: Bounds2D Meters space -> Svg space -> Text
toText viewBox entity = do
  let Bounds2D xBounds yBounds = viewBox
  let Interval x1 x2 = xBounds
  let Interval y1 y2 = yBounds
  let width = x2 .-. x1
  let height = y2 .-. y1
  let attributes =
        [ Attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attribute "version" "1.1"
        , Attribute "width" (lengthText width <> "mm")
        , Attribute "height" (lengthText height <> "mm")
        , Attribute "viewBox" $
            Text.join " " $
              [ lengthText x1
              , lengthText (negative y2)
              , lengthText width
              , lengthText height
              ]
        , blackStroke
        , strokeWidth (Length.pixels 1)
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
write :: Text -> Bounds2D Meters space -> Svg space -> IO ()
write path viewBox entity = IO.writeUtf8 path (toText viewBox entity)

{-| Don't draw anything at all.

Can be useful if you want to conditionally render some content.
-}
nothing :: Svg space
nothing = Empty

-- | Group several SVG entities into a single entity, applying the given attributes to the group.
groupWith :: List (Attribute space) -> List (Svg space) -> Svg space
groupWith = Node "g"

-- | Group several SVG entities into a single entity.
group :: List (Svg space) -> Svg space
group = groupWith []

combine :: (a -> Svg space) -> List a -> Svg space
combine = combineWith []

combineWith :: List (Attribute space) -> (a -> Svg space) -> List a -> Svg space
combineWith attributes function list =
  groupWith attributes (List.map function list)

-- | Draw a line with the given attributes.
lineWith :: List (Attribute space) -> Line2D Meters space -> Svg space
lineWith attributes (Line2D p1 p2) = do
  let Point2D x1 y1 = p1
  let Point2D x2 y2 = p2
  let x1Attribute = Attribute "x1" (lengthText x1)
  let y1Attribute = Attribute "y1" (lengthText (negative y1))
  let x2Attribute = Attribute "x2" (lengthText x2)
  let y2Attribute = Attribute "y2" (lengthText (negative y2))
  Node "line" (x1Attribute : y1Attribute : x2Attribute : y2Attribute : attributes) []

-- | Draw a line.
line :: Line2D Meters space -> Svg space
line = lineWith []

-- | Draw a polyline with the given attributes.
polylineWith :: List (Attribute space) -> Polyline2D Meters space -> Svg space
polylineWith attributes givenPolyline = do
  let points = NonEmpty.toList (Polyline2D.vertices givenPolyline)
  Node "polyline" (noFill : pointsAttribute points : attributes) []

-- | Draw a polyline.
polyline :: Polyline2D Meters space -> Svg space
polyline = polylineWith []

-- | Draw a polygon with the given attributes.
polygonWith :: List (Attribute space) -> Polygon2D Meters space -> Svg space
polygonWith attributes givenPolygon = do
  let points = NonEmpty.toList (Polygon2D.vertices givenPolygon)
  Node "polygon" (pointsAttribute points : attributes) []

-- | Draw a polygon.
polygon :: Polygon2D Meters space -> Svg space
polygon = polygonWith []

-- | Draw a triangle with the given attributes.
triangleWith :: List (Attribute space) -> Triangle2D Meters space -> Svg space
triangleWith attributes (Triangle2D p1 p2 p3) = do
  let vertices = NonEmpty.three p1 p2 p3
  polygonWith attributes (Polygon2D vertices)

-- | Draw a triangle.
triangle :: Triangle2D Meters space -> Svg space
triangle = triangleWith []

-- | Draw a circle with the given attributes, center point and diameter.
circleWith ::
  List (Attribute space) ->
  "centerPoint" ::: Point2D Meters space ->
  "diameter" ::: Length ->
  Svg space
circleWith attributes (Named centerPoint) (Named diameter) = do
  let Point2D cx cy = centerPoint
  let cxAttribute = Attribute "cx" (lengthText cx)
  let cyAttribute = Attribute "cy" (lengthText (negative cy))
  let rAttribute = Attribute "r" (lengthText (0.5 *. diameter))
  Node "circle" (cxAttribute : cyAttribute : rAttribute : attributes) []

-- | Draw a circle with the given center point and diameter.
circle :: "centerPoint" ::: Point2D Meters space -> "diameter" ::: Length -> Svg space
circle = circleWith []

-- | Draw a curve with the given attributes and resolution.
curveWith :: List (Attribute space) -> Resolution Meters -> Curve2D Meters space -> Svg space
curveWith attributes resolution givenCurve =
  polylineWith attributes (Curve2D.toPolyline resolution givenCurve)

-- | Draw a curve with the given resolution.
curve :: Resolution Meters -> Curve2D Meters space -> Svg space
curve = curveWith []

arrow ::
  "start" ::: Point2D Meters space ->
  "end" ::: Point2D Meters space ->
  "headLength" ::: Length ->
  "headWidth" ::: Length ->
  Svg space
arrow = arrowWith []

arrowWith ::
  List (Attribute space) ->
  "start" ::: Point2D Meters space ->
  "end" ::: Point2D Meters space ->
  "headLength" ::: Length ->
  "headWidth" ::: Length ->
  Svg space
arrowWith attributes (Named start) (Named end) (Named headLength) (Named headWidth) =
  case Tolerance.using Quantity.zero (Direction2D.from start end) of
    Error Direction2D.PointsAreCoincident -> nothing
    Ok direction -> do
      let length = Point2D.distanceFrom start end
      let axis = Axis2D start direction
      let frame = Frame2D.fromXAxis axis
      let stemLength = length .-. headLength
      let stemEndPoint = Point2D.along axis stemLength
      let leftPoint = Point2D.placeIn frame (Point2D stemLength (0.5 *. headWidth))
      let rightPoint = Point2D.mirrorAcross axis leftPoint
      let stem = line (Line2D start stemEndPoint)
      let tip = triangle (Triangle2D leftPoint rightPoint end)
      groupWith attributes [stem, tip]

pointsAttribute :: List (Point2D Meters space) -> Attribute space
pointsAttribute givenPoints =
  Attribute "points" (Text.join " " (List.map coordinatesText givenPoints))

coordinatesText :: Point2D Meters space -> Text
coordinatesText (Point2D x y) = lengthText x <> "," <> lengthText (negative y)

lengthText :: Length -> Text
lengthText givenLength = Text.number (Length.inMillimeters givenLength)

-- | Black stroke for curves and borders.
blackStroke :: Attribute space
blackStroke = Attribute "stroke" "black"

-- | Set the stroke color for curves and borders.
strokeColor :: Color -> Attribute space
strokeColor color = Attribute "stroke" (Color.toHex color)

strokeWidth :: Length -> Attribute space
strokeWidth givenWidth = Attribute "stroke-width" (lengthText givenWidth)

-- | Set shapes to have no fill.
noFill :: Attribute space
noFill = Attribute "fill" "none"

-- | Set shapes to have white fill.
whiteFill :: Attribute space
whiteFill = Attribute "fill" "white"

-- | Set the fill color for shapes.
fillColor :: Color -> Attribute space
fillColor color = Attribute "fill" (Color.toHex color)

opacity :: Number -> Attribute space
opacity value = Attribute "opacity" (Text.number value)

miterStrokeJoins :: Attribute space
miterStrokeJoins = Attribute "stroke-linejoin" "miter"

roundStrokeJoins :: Attribute space
roundStrokeJoins = Attribute "stroke-linejoin" "round"

bevelStrokeJoins :: Attribute space
bevelStrokeJoins = Attribute "stroke-linejoin" "bevel"

noStrokeCaps :: Attribute space
noStrokeCaps = Attribute "stroke-linecap" "butt"

roundStrokeCaps :: Attribute space
roundStrokeCaps = Attribute "stroke-linecap" "round"

squareStrokeCaps :: Attribute space
squareStrokeCaps = Attribute "stroke-linecap" "square"
