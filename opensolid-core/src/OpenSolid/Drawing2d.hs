module OpenSolid.Drawing2d
  ( Drawing2d
  , Attribute
  , Resolution
  , Point
  , (>>)
  , toSvg
  , writeSvg
  , nothing
  , group
  , groupWith
  , collect
  , collectWith
  , line
  , lineWith
  , polyline
  , polylineWith
  , polygon
  , polygonWith
  , circle
  , circleWith
  , curve
  , curveWith
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
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Color (Color)
import OpenSolid.Color qualified as Color
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Polyline2d (Polyline2d)
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

type Drawing2d :: Type -> Type

-- | A 2D drawing composed of shapes with attributes such as colour and stroke width.
data Drawing2d space = Empty | Node Text (List (Attribute space)) (List (Drawing2d space))

data Attribute space = Attribute Text Text deriving (Show)

type Resolution = ?resolution :: Length

type Point space = Point2d (space @ Meters)

instance FFI (Drawing2d space) where
  representation = FFI.classRepresentation "Drawing2d"

instance FFI (Attribute space) where
  representation = FFI.nestedClassRepresentation "Drawing2d" "Attribute"

instance Composition (Drawing2d space) (Drawing2d space) (Drawing2d space) where
  drawing >> Node "g" [] drawings = Node "g" [] (drawing : drawings)
  drawing1 >> drawing2 = Node "g" [] [drawing1, drawing2]

drawingText :: Text -> Drawing2d space -> Maybe Text
drawingText _ Empty = Nothing
drawingText indent (Node name attributes children) = do
  let attributeLines = List.map (attributeText ("\n" <> indent <> "   ")) attributes
  let openingTag = indent <> "<" <> name <> Text.concat attributeLines <> ">"
  let childLines = Maybe.collect (drawingText (indent <> "  ")) children
  let closingTag = indent <> "</" <> name <> ">"
  Just (Text.multiline (openingTag : childLines) <> "\n" <> closingTag)

attributeText :: Text -> Attribute space -> Text
attributeText indent (Attribute name value) = Text.concat [indent, name, "=\"", value, "\""]

{-| Render a drawing to SVG.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.
-}
toSvg :: Bounds2d (space @ Meters) -> Drawing2d space -> Text
toSvg viewBox drawing = do
  let Bounds2d xBounds yBounds = viewBox
  let Bounds x1 x2 = xBounds
  let Bounds y1 y2 = yBounds
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
    , Maybe.withDefault "" (drawingText "" (Node "svg" attributes [drawing])) <> "\n"
    ]

{-| Render SVG to a file.

The given bounding box defines the overall size of the drawing;
anything outside of this will be cropped.
-}
writeSvg :: Text -> Bounds2d (space @ Meters) -> Drawing2d space -> IO ()
writeSvg path viewBox drawing = IO.writeUtf8 path (toSvg viewBox drawing)

nothing :: Drawing2d space
nothing = Empty

-- | Group several drawings into a single drawing, applying the given attributes to the group.
groupWith :: List (Attribute space) -> List (Drawing2d space) -> Drawing2d space
groupWith = Node "g"

-- | Group several drawings into a single drawing.
group :: List (Drawing2d space) -> Drawing2d space
group = groupWith []

collect :: Foldable list => (a -> Drawing2d space) -> list a -> Drawing2d space
collect = collectWith []

collectWith ::
  Foldable list =>
  List (Attribute space) ->
  (a -> Drawing2d space) ->
  list a ->
  Drawing2d space
collectWith attributes function list =
  groupWith attributes (List.map function (Data.Foldable.toList list))

lineWith :: List (Attribute space) -> Point space -> Point space -> Drawing2d space
lineWith attributes (Point2d x1 y1) (Point2d x2 y2) = do
  let x1Attribute = Attribute "x1" (lengthText x1)
  let y1Attribute = Attribute "y1" (lengthText -y1)
  let x2Attribute = Attribute "x2" (lengthText x2)
  let y2Attribute = Attribute "y2" (lengthText -y2)
  Node "line" (x1Attribute : y1Attribute : x2Attribute : y2Attribute : attributes) []

line :: Point space -> Point space -> Drawing2d space
line = lineWith []

polylineWith ::
  Vertex2d vertex (space @ Meters) =>
  List (Attribute space) ->
  Polyline2d vertex ->
  Drawing2d space
polylineWith attributes givenPolyline = do
  let vertices = List.map Vertex2d.position (NonEmpty.toList givenPolyline.vertices)
  Node "polyline" (noFill : pointsAttribute vertices : attributes) []

polyline :: Vertex2d vertex (space @ Meters) => Polyline2d vertex -> Drawing2d space
polyline = polylineWith []

-- | Create a polygon with the given attributes and vertices.
polygonWith :: List (Attribute space) -> List (Point space) -> Drawing2d space
polygonWith attributes vertices =
  Node "polygon" (pointsAttribute vertices : attributes) []

-- | Create a polygon with the given vertices.
polygon :: List (Point space) -> Drawing2d space
polygon = polygonWith []

-- | Create a circle with the given attributes, center point and diameter.
circleWith ::
  List (Attribute space) ->
  ("centerPoint" ::: Point space, "diameter" ::: Length) ->
  Drawing2d space
circleWith attributes (Field centerPoint, Field diameter) = do
  let Point2d cx cy = centerPoint
  let cxAttribute = Attribute "cx" (lengthText cx)
  let cyAttribute = Attribute "cy" (lengthText -cy)
  let rAttribute = Attribute "r" (lengthText (0.5 * diameter))
  Node "circle" (cxAttribute : cyAttribute : rAttribute : attributes) []

-- | Create a circle with the given center point and diameter.
circle :: ("centerPoint" ::: Point space, "diameter" ::: Length) -> Drawing2d space
circle = circleWith []

-- | Draw a curve with the given attributes and accuracy.
curveWith :: List (Attribute space) -> Length -> Curve2d (space @ Meters) -> Drawing2d space
curveWith attributes maxError givenCurve = do
  let approximation = Curve2d.toPolyline maxError givenCurve
  polylineWith attributes approximation

-- | Draw a curve with the given attributes and accuracy.
curve :: Length -> Curve2d (space @ Meters) -> Drawing2d space
curve = curveWith []

pointsAttribute :: List (Point space) -> Attribute space
pointsAttribute givenPoints =
  Attribute "points" (Text.join " " (List.map coordinatesText givenPoints))

coordinatesText :: Point space -> Text
coordinatesText (Point2d x y) = lengthText x <> "," <> lengthText -y

lengthText :: Length -> Text
lengthText givenLength = Text.float (Length.inMillimeters givenLength)

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

opacity :: Float -> Attribute space
opacity value = Attribute "opacity" (Text.float value)

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
