module OpenSolid.Color
  ( Color
  , rgbInt
  , rgbFloat
  , hsl
  , fromHex
  , toHex
  , rgbIntComponents
  , rgbFloatComponents
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
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

-- | An RGB color value.
type Color = Data.Colour.Colour Float

-- | Construct a color from its RGB components, in the range [0,1].
rgbFloat :: Float -> Float -> Float -> Color
rgbFloat = Data.Colour.SRGB.sRGB

-- | Construct a color from its RGB components, in the range [0,255].
rgbInt :: Int -> Int -> Int -> Color
rgbInt r g b = rgbFloat (r / 255) (g / 255) (b / 255)

-- | Construct a color from its hue, saturation and lightness values.
hsl :: Angle -> Float -> Float -> Color
hsl hue saturation lightness =
  Data.Colour.RGBSpace.HSL.hsl (Angle.inDegrees hue) saturation lightness
    |> Data.Colour.RGBSpace.uncurryRGB rgbFloat

-- | Construct a color from a hex string such as '#f3f3f3' or 'f3f3f3'.
fromHex :: Text -> Color
fromHex text = Data.Colour.SRGB.sRGB24read (Text.unpack text)

-- | Convert a color to a hex string such as '#f3f3f3'.
toHex :: Color -> Text
toHex color = Text.pack (Data.Colour.SRGB.sRGB24show color)

-- | Get the RGB components of a color as values in the range [0,1].
rgbFloatComponents :: Color -> (Float, Float, Float)
rgbFloatComponents = (.rgbFloatComponents)

-- | Get the RGB components of a color as values in the range [0,255].
rgbIntComponents :: Color -> (Int, Int, Int)
rgbIntComponents = (.rgbIntComponents)

-- | Light Scarlet Red from the Tango icon theme.
lightRed :: Color
lightRed = rgbInt 239 41 41

-- | Scarlet Red from the Tango icon theme.
red :: Color
red = rgbInt 204 0 0

-- | Dark Scarlet Red from the Tango icon theme.
darkRed :: Color
darkRed = rgbInt 164 0 0

-- | Light Orange from the Tango icon theme.
lightOrange :: Color
lightOrange = rgbInt 252 175 62

-- | Orange from the Tango icon theme.
orange :: Color
orange = rgbInt 245 121 0

-- | Dark Orange from the Tango icon theme.
darkOrange :: Color
darkOrange = rgbInt 206 92 0

-- | Light Butter from the Tango icon theme.
lightYellow :: Color
lightYellow = rgbInt 255 233 79

-- | Butter from the Tango icon theme.
yellow :: Color
yellow = rgbInt 237 212 0

-- | Dark Butter from the Tango icon theme.
darkYellow :: Color
darkYellow = rgbInt 196 160 0

-- | Light Chameleon from the Tango icon theme.
lightGreen :: Color
lightGreen = rgbInt 138 226 52

-- | Chameleon from the Tango icon theme.
green :: Color
green = rgbInt 115 210 22

-- | Dark Chameleon from the Tango icon theme.
darkGreen :: Color
darkGreen = rgbInt 78 154 6

-- | Light Sky Blue from the Tango icon theme.
lightBlue :: Color
lightBlue = rgbInt 114 159 207

-- | Sky Blue from the Tango icon theme.
blue :: Color
blue = rgbInt 52 101 164

-- | Dark Sky Blue from the Tango icon theme.
darkBlue :: Color
darkBlue = rgbInt 32 74 135

-- | Light Plum from the Tango icon theme.
lightPurple :: Color
lightPurple = rgbInt 173 127 168

-- | Plum from the Tango icon theme.
purple :: Color
purple = rgbInt 117 80 123

-- | Dark Plum from the Tango icon theme.
darkPurple :: Color
darkPurple = rgbInt 92 53 102

-- | Light Chocolate from the Tango icon theme.
lightBrown :: Color
lightBrown = rgbInt 233 185 110

-- | Chocolate from the Tango icon theme.
brown :: Color
brown = rgbInt 193 125 17

-- | Dark Chocolate from the Tango icon theme.
darkBrown :: Color
darkBrown = rgbInt 143 89 2

-- | Black.
black :: Color
black = rgbInt 0 0 0

-- | White.
white :: Color
white = rgbInt 255 255 255

-- | Aluminium 1/6 from the Tango icon theme.
lightGrey :: Color
lightGrey = rgbInt 238 238 236

-- | Aluminium 2/6 from the Tango icon theme.
grey :: Color
grey = rgbInt 211 215 207

-- | Aluminium 3/6 from the Tango icon theme.
darkGrey :: Color
darkGrey = rgbInt 186 189 182

-- | Aluminium 1/6 from the Tango icon theme.
lightGray :: Color
lightGray = rgbInt 238 238 236

-- | Aluminium 2/6 from the Tango icon theme.
gray :: Color
gray = rgbInt 211 215 207

-- | Aluminium 3/6 from the Tango icon theme.
darkGray :: Color
darkGray = rgbInt 186 189 182

-- | Aluminium 4/6 from the Tango icon theme.
lightCharcoal :: Color
lightCharcoal = rgbInt 136 138 133

-- | Aluminium 5/6 from the Tango icon theme.
charcoal :: Color
charcoal = rgbInt 85 87 83

-- | Aluminium 6/6 from the Tango icon theme.
darkCharcoal :: Color
darkCharcoal = rgbInt 46 52 54
