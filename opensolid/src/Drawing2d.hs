module Drawing2d
  ( Entity
  , Attribute
  , Resolution
  , toSvg
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Length (Length)
import Length qualified
import List qualified
import OpenSolid
import Range (Range (Range))
import String qualified
import Units (Meters)

data Entity space = Node String (List Attribute) (List (Entity space))

data Attribute = Attribute String String

type Resolution = ?resolution :: Length

entityString :: String -> Entity space -> String
entityString indent (Node name attributes children) =
  let openingLine = indent ++ "<" ++ name
      attributeLines = List.map (attributeString (indent ++ "   ")) attributes
      childLines = String.join "\n" (List.map (entityString (indent ++ "  ")) children)
      closingLine = "</" ++ name ++ ">\n"
   in String.join "\n" $
        [ openingLine
        , String.join "\n" attributeLines ++ ">"
        , childLines
        , closingLine
        ]

attributeString :: String -> Attribute -> String
attributeString indent (Attribute name value) = String.concat [indent, name, "=\"", value, "\""]

toSvg :: Bounds2d (space @ Meters) -> List (Entity space) -> String
toSvg (Bounds2d (Range x1 x2) (Range y1 y2)) entities =
  let width = Length.inMillimeters (x2 - x1)
      height = Length.inMillimeters (y2 - y1)
      attributes =
        [ Attribute "xmlns" "http://www.w3.org/2000/svg"
        , Attribute "version" "1.1"
        , Attribute "width" (String.fromFloat width ++ "mm")
        , Attribute "height" (String.fromFloat height ++ "mm")
        , Attribute "viewBox" $
            String.join " " $
              [ String.fromFloat (Length.inMillimeters x1)
              , String.fromFloat (Length.inMillimeters -y2)
              , String.fromFloat width
              , String.fromFloat height
              ]
        ]
   in String.join "\n" $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
        , entityString "" (Node "svg" attributes entities)
        ]
