module Drawing2d
  ( Entity
  , Attribute
  , Resolution
  , Point
  , toSvg
  , writeTo
  , group
  , polyline
  , polygon
  , blackStroke
  , strokeWidth
  , noFill
  , fillColour
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Colour (Colour)
import Colour qualified
import File qualified
import Length (Length)
import Length qualified
import List qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Range (Range (Range))
import String qualified
import Units (Meters)

data Entity space = Node String (List (Attribute space)) (List (Entity space))

data Attribute space = Attribute String String

type Resolution = ?resolution :: Length

type Point space = Point2d (space @ Meters)

entityString :: String -> Entity space -> String
entityString indent (Node name attributes children) =
  let openingTag = indent ++ "<" ++ name ++ String.concat attributeLines ++ ">"
      attributeLines = List.map (attributeString ("\n" ++ indent ++ "   ")) attributes
      childLines = List.map (entityString (indent ++ "  ")) children
      closingTag = indent ++ "</" ++ name ++ ">"
   in String.join "\n" (openingTag : childLines) ++ "\n" ++ closingTag

attributeString :: String -> Attribute space -> String
attributeString indent (Attribute name value) = String.concat [indent, name, "=\"", value, "\""]

toSvg :: Bounds2d (space @ Meters) -> List (Entity space) -> String
toSvg (Bounds2d (Range x1 x2) (Range y1 y2)) entities =
  let width = x2 - x1
      height = y2 - y1
      attributes =
        [ Attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attribute "version" "1.1"
        , Attribute "width" (lengthString width ++ "mm")
        , Attribute "height" (lengthString height ++ "mm")
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
   in String.join "\n" $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        , entityString "" (Node "svg" attributes entities) ++ "\n"
        ]

writeTo :: String -> Bounds2d (space @ Meters) -> List (Entity space) -> Task String ()
writeTo path viewBox entities = File.writeTo path (toSvg viewBox entities)

group :: List (Attribute space) -> List (Entity space) -> Entity space
group attributes children = Node "g" attributes children

polyline :: List (Attribute space) -> List (Point space) -> Entity space
polyline attributes vertices =
  Node "polyline" (noFill : pointsAttribute vertices : attributes) []

polygon :: List (Attribute space) -> List (Point space) -> Entity space
polygon attributes vertices =
  Node "polygon" (pointsAttribute vertices : attributes) []

pointsAttribute :: List (Point space) -> Attribute space
pointsAttribute givenPoints =
  Attribute "points" (String.join " " (List.map coordinatesString givenPoints))

coordinatesString :: Point space -> String
coordinatesString (Point2d x y) = lengthString x ++ "," ++ lengthString -y

lengthString :: Length -> String
lengthString givenLength = String.fromFloat (Length.inMillimeters givenLength)

blackStroke :: Attribute space
blackStroke = Attribute "stroke" "black"

strokeWidth :: Length -> Attribute space
strokeWidth givenWidth = Attribute "stroke-width" (lengthString givenWidth)

noFill :: Attribute space
noFill = Attribute "fill" "none"

fillColour :: Colour -> Attribute space
fillColour colour = Attribute "fill" (Colour.toHexString colour)
