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
import File qualified
import Length (Length)
import Length qualified
import List qualified
import Maybe qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range qualified
import String qualified
import Units (Meters)

type Entity :: Type -> Type
data Entity space = Empty | Node String (List (Attribute space)) (List (Entity space))

data Attribute space = Attribute String String

type Resolution = ?resolution :: Length

type Point space = Point2d (space @ Meters)

entityString :: String -> Entity space -> Maybe String
entityString _ Empty = Nothing
entityString indent (Node name attributes children) = do
  let attributeLines = List.map (attributeString ("\n" + indent + "   ")) attributes
  let openingTag = indent + "<" + name + String.concat attributeLines + ">"
  let childLines = Maybe.collect (entityString (indent + "  ")) children
  let closingTag = indent + "</" + name + ">"
  Just (String.multiline (openingTag : childLines) + "\n" + closingTag)

attributeString :: String -> Attribute space -> String
attributeString indent (Attribute name value) = String.concat [indent, name, "=\"", value, "\""]

toSvg :: Bounds2d (space @ Meters) -> List (Entity space) -> String
toSvg viewBox entities = do
  let (xRange, yRange) = Bounds2d.coordinates viewBox
  let (x1, x2) = Range.endpoints xRange
  let (y1, y2) = Range.endpoints yRange
  let width = x2 - x1
  let height = y2 - y1
  let attributes =
        [ Attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attribute "version" "1.1"
        , Attribute "width" (lengthString width + "mm")
        , Attribute "height" (lengthString height + "mm")
        , Attribute "viewBox" $
            String.join " " $
              [ lengthString x1
              , lengthString -y2
              , lengthString width
              , lengthString height
              ]
        , blackStroke
        , strokeWidth (Length.pixels 1.0)
        , noFill
        ]
  String.multiline
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    , Maybe.withDefault "" (entityString "" (Node "svg" attributes entities)) + "\n"
    ]

writeTo :: String -> Bounds2d (space @ Meters) -> List (Entity space) -> IO ()
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
  let x1Attribute = Attribute "x1" (lengthString x1)
  let y1Attribute = Attribute "y1" (lengthString -y1)
  let x2Attribute = Attribute "x2" (lengthString x2)
  let y2Attribute = Attribute "y2" (lengthString -y2)
  Node "line" (x1Attribute : y1Attribute : x2Attribute : y2Attribute : attributes) []

polyline :: List (Attribute space) -> List (Point space) -> Entity space
polyline attributes vertices =
  Node "polyline" (noFill : pointsAttribute vertices : attributes) []

polygon :: List (Attribute space) -> List (Point space) -> Entity space
polygon attributes vertices =
  Node "polygon" (pointsAttribute vertices : attributes) []

circle :: List (Attribute space) -> Point space -> Length -> Entity space
circle attributes centerPoint radius = do
  let (cx, cy) = Point2d.coordinates centerPoint
  let cxAttribute = Attribute "cx" (lengthString cx)
  let cyAttribute = Attribute "cy" (lengthString -cy)
  let rAttribute = Attribute "r" (lengthString radius)
  Node "circle" (cxAttribute : cyAttribute : rAttribute : attributes) []

pointsAttribute :: List (Point space) -> Attribute space
pointsAttribute givenPoints =
  Attribute "points" (String.join " " (List.map coordinatesString givenPoints))

coordinatesString :: Point space -> String
coordinatesString point = do
  let (x, y) = Point2d.coordinates point
  lengthString x + "," + lengthString -y

lengthString :: Length -> String
lengthString givenLength = String.fromFloat (Length.inMillimeters givenLength)

blackStroke :: Attribute space
blackStroke = Attribute "stroke" "black"

strokeColour :: Colour -> Attribute space
strokeColour colour = Attribute "stroke" (Colour.toHexString colour)

strokeWidth :: Length -> Attribute space
strokeWidth givenWidth = Attribute "stroke-width" (lengthString givenWidth)

noFill :: Attribute space
noFill = Attribute "fill" "none"

fillColour :: Colour -> Attribute space
fillColour colour = Attribute "fill" (Colour.toHexString colour)

opacity :: Float -> Attribute space
opacity value = Attribute "opacity" (String.fromFloat value)
