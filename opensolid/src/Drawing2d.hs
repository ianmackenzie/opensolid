module Drawing2d
  ( Entity
  , Attribute
  , Resolution
  , Point
  , toSvg
  , writeTo
  , nothing
  , group
  , with
  , line
  , polyline
  , polygon
  , circle
  , curve
  , blackStroke
  , strokeColour
  , strokeWidth
  , noFill
  , fillColour
  , opacity
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Colour (Colour)
import Colour qualified
import Curve2d (Curve2d)
import Curve2d qualified
import File qualified
import Length (Length)
import Length qualified
import List qualified
import Maybe qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Polyline2d (Polyline2d)
import Polyline2d qualified
import Range qualified
import Text qualified
import Units (Meters)
import Vertex2d (Vertex2d)
import Vertex2d qualified

type Entity :: Type -> Type
data Entity space = Empty | Node Text (List (Attribute space)) (List (Entity space))

data Attribute space = Attribute Text Text

type Resolution = ?resolution :: Length

type Point space = Point2d (space @ Meters)

entityText :: Text -> Entity space -> Maybe Text
entityText _ Empty = Nothing
entityText indent (Node name attributes children) = do
  let attributeLines = List.map (attributeText ("\n" + indent + "   ")) attributes
  let openingTag = indent + "<" + name + Text.concat attributeLines + ">"
  let childLines = Maybe.collect (entityText (indent + "  ")) children
  let closingTag = indent + "</" + name + ">"
  Just (Text.multiline (openingTag : childLines) + "\n" + closingTag)

attributeText :: Text -> Attribute space -> Text
attributeText indent (Attribute name value) = Text.concat [indent, name, "=\"", value, "\""]

toSvg :: Bounds2d (space @ Meters) -> List (Entity space) -> Text
toSvg viewBox entities = do
  let (xRange, yRange) = Bounds2d.coordinates viewBox
  let (x1, x2) = Range.endpoints xRange
  let (y1, y2) = Range.endpoints yRange
  let width = x2 - x1
  let height = y2 - y1
  let attributes =
        [ Attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attribute "version" "1.1"
        , Attribute "width" (lengthText width + "mm")
        , Attribute "height" (lengthText height + "mm")
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
    , Maybe.withDefault "" (entityText "" (Node "svg" attributes entities)) + "\n"
    ]

writeTo :: Text -> Bounds2d (space @ Meters) -> List (Entity space) -> IO ()
writeTo path viewBox entities = File.writeTo path (toSvg viewBox entities)

nothing :: Entity space
nothing = Empty

with :: List (Attribute space) -> List (Entity space) -> Entity space
with attributes children = Node "g" attributes children

group :: List (Entity space) -> Entity space
group = with []

line :: List (Attribute space) -> Point space -> Point space -> Entity space
line attributes p1 p2 = do
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  let x1Attribute = Attribute "x1" (lengthText x1)
  let y1Attribute = Attribute "y1" (lengthText -y1)
  let x2Attribute = Attribute "x2" (lengthText x2)
  let y2Attribute = Attribute "y2" (lengthText -y2)
  Node "line" (x1Attribute : y1Attribute : x2Attribute : y2Attribute : attributes) []

polyline ::
  Vertex2d vertex (space @ Meters) =>
  List (Attribute space) ->
  Polyline2d vertex ->
  Entity space
polyline attributes givenPolyline = do
  let vertices = List.map Vertex2d.position (NonEmpty.toList (Polyline2d.vertices givenPolyline))
  Node "polyline" (noFill : pointsAttribute vertices : attributes) []

polygon :: List (Attribute space) -> List (Point space) -> Entity space
polygon attributes vertices =
  Node "polygon" (pointsAttribute vertices : attributes) []

circle :: List (Attribute space) -> Point space -> Length -> Entity space
circle attributes centerPoint radius = do
  let (cx, cy) = Point2d.coordinates centerPoint
  let cxAttribute = Attribute "cx" (lengthText cx)
  let cyAttribute = Attribute "cy" (lengthText -cy)
  let rAttribute = Attribute "r" (lengthText radius)
  Node "circle" (cxAttribute : cyAttribute : rAttribute : attributes) []

curve ::
  List (Attribute space) ->
  Length ->
  Curve2d (space @ Meters) ->
  Entity space
curve attributes maxError givenCurve = do
  let approximation = Curve2d.toPolyline maxError (Curve2d.pointOn givenCurve) givenCurve
  polyline attributes approximation

pointsAttribute :: List (Point space) -> Attribute space
pointsAttribute givenPoints =
  Attribute "points" (Text.join " " (List.map coordinatesText givenPoints))

coordinatesText :: Point space -> Text
coordinatesText point = do
  let (x, y) = Point2d.coordinates point
  lengthText x + "," + lengthText -y

lengthText :: Length -> Text
lengthText givenLength = Text.float (Length.inMillimeters givenLength)

blackStroke :: Attribute space
blackStroke = Attribute "stroke" "black"

strokeColour :: Colour -> Attribute space
strokeColour colour = Attribute "stroke" (Colour.toHex colour)

strokeWidth :: Length -> Attribute space
strokeWidth givenWidth = Attribute "stroke-width" (lengthText givenWidth)

noFill :: Attribute space
noFill = Attribute "fill" "none"

fillColour :: Colour -> Attribute space
fillColour colour = Attribute "fill" (Colour.toHex colour)

opacity :: Float -> Attribute space
opacity value = Attribute "opacity" (Text.float value)
