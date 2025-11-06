module OpenSolid.Color
  ( Color
  , rgb1
  , rgb255
  , hsl1
  , hex
  , toRgb1
  , toRgb255
  , toHsl1
  , toHex
  , lightRed
  , red
  , darkRed
  , lightOrange
  , orange
  , darkOrange
  , lightYellow
  , yellow
  , darkYellow
  , lightGreen
  , green
  , darkGreen
  , lightBlue
  , blue
  , darkBlue
  , lightPurple
  , purple
  , darkPurple
  , lightBrown
  , brown
  , darkBrown
  , black
  , white
  , lightGrey
  , grey
  , darkGrey
  , lightGray
  , gray
  , darkGray
  , lightCharcoal
  , charcoal
  , darkCharcoal
  )
where

import Data.Colour qualified
import Data.Colour.RGBSpace qualified
import Data.Colour.RGBSpace.HSL qualified
import Data.Colour.SRGB qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding ((/))
import OpenSolid.Text qualified as Text
import Prelude ((/))

-- | An RGB color value.
type Color = Data.Colour.Colour Number

-- | Construct a color from its RGB components, in the range [0,1].
rgb1 :: Number -> Number -> Number -> Color
rgb1 = Data.Colour.SRGB.sRGB

-- | Construct a color from its RGB components, in the range [0,255].
rgb255 :: Int -> Int -> Int -> Color
rgb255 r g b =
  rgb1
    (Number.fromInt r ./ 255.0)
    (Number.fromInt g ./ 255.0)
    (Number.fromInt b ./ 255.0)

{-| Construct a color from its hue, saturation and lightness values.

Saturation and lightness should be in the range [0,1].
-}
hsl1 :: Angle -> Number -> Number -> Color
hsl1 hue saturation lightness =
  Data.Colour.RGBSpace.HSL.hsl (Angle.inDegrees hue) saturation lightness
    |> Data.Colour.RGBSpace.uncurryRGB rgb1

-- | Construct a color from a hex string such as '#f3f3f3' or 'f3f3f3'.
hex :: Text -> Color
hex text = Data.Colour.SRGB.sRGB24read (Text.unpack text)

-- | Get the RGB components of a color as values in the range [0,1].
toRgb1 :: Color -> (Number, Number, Number)
toRgb1 color = do
  let Data.Colour.SRGB.RGB r g b = Data.Colour.SRGB.toSRGB color
  (r, g, b)

-- | Get the RGB components of a color as values in the range [0,255].
toRgb255 :: Color -> (Int, Int, Int)
toRgb255 color = do
  let (r, g, b) = toRgb1 color
  let toInt component = Number.round (component .* 255.0)
  (toInt r, toInt g, toInt b)

{-| Get the hue, saturation and lightness of a color.

The returned saturation and lightness will be in the range [0,1].
-}
toHsl1 :: Color -> (Angle, Number, Number)
toHsl1 color = do
  let (h, s, l) = Data.Colour.RGBSpace.HSL.hslView (Data.Colour.SRGB.toSRGB color)
  (Angle.degrees h, s, l)

-- | Convert a color to a hex string such as '#f3f3f3'.
toHex :: Color -> Text
toHex color = Text.pack (Data.Colour.SRGB.sRGB24show color)

-- | Light Scarlet Red from the Tango icon theme.
lightRed :: Color
lightRed = rgb255 239 41 41

-- | Scarlet Red from the Tango icon theme.
red :: Color
red = rgb255 204 0 0

-- | Dark Scarlet Red from the Tango icon theme.
darkRed :: Color
darkRed = rgb255 164 0 0

-- | Light Orange from the Tango icon theme.
lightOrange :: Color
lightOrange = rgb255 252 175 62

-- | Orange from the Tango icon theme.
orange :: Color
orange = rgb255 245 121 0

-- | Dark Orange from the Tango icon theme.
darkOrange :: Color
darkOrange = rgb255 206 92 0

-- | Light Butter from the Tango icon theme.
lightYellow :: Color
lightYellow = rgb255 255 233 79

-- | Butter from the Tango icon theme.
yellow :: Color
yellow = rgb255 237 212 0

-- | Dark Butter from the Tango icon theme.
darkYellow :: Color
darkYellow = rgb255 196 160 0

-- | Light Chameleon from the Tango icon theme.
lightGreen :: Color
lightGreen = rgb255 138 226 52

-- | Chameleon from the Tango icon theme.
green :: Color
green = rgb255 115 210 22

-- | Dark Chameleon from the Tango icon theme.
darkGreen :: Color
darkGreen = rgb255 78 154 6

-- | Light Sky Blue from the Tango icon theme.
lightBlue :: Color
lightBlue = rgb255 114 159 207

-- | Sky Blue from the Tango icon theme.
blue :: Color
blue = rgb255 52 101 164

-- | Dark Sky Blue from the Tango icon theme.
darkBlue :: Color
darkBlue = rgb255 32 74 135

-- | Light Plum from the Tango icon theme.
lightPurple :: Color
lightPurple = rgb255 173 127 168

-- | Plum from the Tango icon theme.
purple :: Color
purple = rgb255 117 80 123

-- | Dark Plum from the Tango icon theme.
darkPurple :: Color
darkPurple = rgb255 92 53 102

-- | Light Chocolate from the Tango icon theme.
lightBrown :: Color
lightBrown = rgb255 233 185 110

-- | Chocolate from the Tango icon theme.
brown :: Color
brown = rgb255 193 125 17

-- | Dark Chocolate from the Tango icon theme.
darkBrown :: Color
darkBrown = rgb255 143 89 2

-- | Black.
black :: Color
black = rgb255 0 0 0

-- | White.
white :: Color
white = rgb255 255 255 255

-- | Aluminium 1/6 from the Tango icon theme.
lightGrey :: Color
lightGrey = rgb255 238 238 236

-- | Aluminium 2/6 from the Tango icon theme.
grey :: Color
grey = rgb255 211 215 207

-- | Aluminium 3/6 from the Tango icon theme.
darkGrey :: Color
darkGrey = rgb255 186 189 182

-- | Aluminium 1/6 from the Tango icon theme.
lightGray :: Color
lightGray = rgb255 238 238 236

-- | Aluminium 2/6 from the Tango icon theme.
gray :: Color
gray = rgb255 211 215 207

-- | Aluminium 3/6 from the Tango icon theme.
darkGray :: Color
darkGray = rgb255 186 189 182

-- | Aluminium 4/6 from the Tango icon theme.
lightCharcoal :: Color
lightCharcoal = rgb255 136 138 133

-- | Aluminium 5/6 from the Tango icon theme.
charcoal :: Color
charcoal = rgb255 85 87 83

-- | Aluminium 6/6 from the Tango icon theme.
darkCharcoal :: Color
darkCharcoal = rgb255 46 52 54
