module Colour
  ( Colour
  , rgb
  , rgb255
  , hsl
  , fromHex
  , toHex
  , components
  , components255
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

import Angle qualified
import Data.Colour qualified
import Data.Colour.RGBSpace qualified
import Data.Colour.RGBSpace.HSL qualified
import Data.Colour.SRGB qualified
import Float qualified
import OpenSolid
import Text qualified

-- | An RGB color value.
type Colour = Data.Colour.Colour Float

-- | Construct a color from its RGB components, in the range [0,1].
rgb :: Float -> Float -> Float -> Colour
rgb = Data.Colour.SRGB.sRGB

-- | Construct a color from its RGB components, in the range [0,255].
rgb255 :: Int -> Int -> Int -> Colour
rgb255 r g b = rgb (r / 255) (g / 255) (b / 255)

-- | Construct a color from its hue, saturation and lightness values.
hsl :: Angle -> Float -> Float -> Colour
hsl hue saturation lightness =
  Data.Colour.RGBSpace.HSL.hsl (Angle.inDegrees hue) saturation lightness
    |> Data.Colour.RGBSpace.uncurryRGB rgb

-- | Construct a color from a hex string such as '#f3f3f3' or 'f3f3f3'.
fromHex :: Text -> Colour
fromHex = Text.unpack >> Data.Colour.SRGB.sRGB24read

-- | Convert a color to a hex string such as '#f3f3f3'.
toHex :: Colour -> Text
toHex = Data.Colour.SRGB.sRGB24show >> Text.pack

-- | Get the RGB components of a color as values in the range [0,1].
components :: Colour -> (Float, Float, Float)
components colour = do
  let (Data.Colour.SRGB.RGB r g b) = Data.Colour.SRGB.toSRGB colour
  (r, g, b)

-- | Get the RGB components of a color as values in the range [0,255].
components255 :: Colour -> (Int, Int, Int)
components255 colour = do
  let (r, g, b) = components colour
  (Float.round (r * 255.0), Float.round (g * 255.0), Float.round (b * 255.0))

-- | Light Scarlet Red from the Tango icon theme.
lightRed :: Colour
lightRed = rgb255 239 41 41

-- | Scarlet Red from the Tango icon theme.
red :: Colour
red = rgb255 204 0 0

-- | Dark Scarlet Red from the Tango icon theme.
darkRed :: Colour
darkRed = rgb255 164 0 0

-- | Light Orange from the Tango icon theme.
lightOrange :: Colour
lightOrange = rgb255 252 175 62

-- | Orange from the Tango icon theme.
orange :: Colour
orange = rgb255 245 121 0

-- | Dark Orange from the Tango icon theme.
darkOrange :: Colour
darkOrange = rgb255 206 92 0

-- | Light Butter from the Tango icon theme.
lightYellow :: Colour
lightYellow = rgb255 255 233 79

-- | Butter from the Tango icon theme.
yellow :: Colour
yellow = rgb255 237 212 0

-- | Dark Butter from the Tango icon theme.
darkYellow :: Colour
darkYellow = rgb255 196 160 0

-- | Light Chameleon from the Tango icon theme.
lightGreen :: Colour
lightGreen = rgb255 138 226 52

-- | Chameleon from the Tango icon theme.
green :: Colour
green = rgb255 115 210 22

-- | Dark Chameleon from the Tango icon theme.
darkGreen :: Colour
darkGreen = rgb255 78 154 6

-- | Light Sky Blue from the Tango icon theme.
lightBlue :: Colour
lightBlue = rgb255 114 159 207

-- | Sky Blue from the Tango icon theme.
blue :: Colour
blue = rgb255 52 101 164

-- | Dark Sky Blue from the Tango icon theme.
darkBlue :: Colour
darkBlue = rgb255 32 74 135

-- | Light Plum from the Tango icon theme.
lightPurple :: Colour
lightPurple = rgb255 173 127 168

-- | Plum from the Tango icon theme.
purple :: Colour
purple = rgb255 117 80 123

-- | Dark Plum from the Tango icon theme.
darkPurple :: Colour
darkPurple = rgb255 92 53 102

-- | Light Chocolate from the Tango icon theme.
lightBrown :: Colour
lightBrown = rgb255 233 185 110

-- | Chocolate from the Tango icon theme.
brown :: Colour
brown = rgb255 193 125 17

-- | Dark Chocolate from the Tango icon theme.
darkBrown :: Colour
darkBrown = rgb255 143 89 2

-- | Black.
black :: Colour
black = rgb255 0 0 0

-- | White.
white :: Colour
white = rgb255 255 255 255

-- | Aluminium 1/6 from the Tango icon theme.
lightGrey :: Colour
lightGrey = rgb255 238 238 236

-- | Aluminium 2/6 from the Tango icon theme.
grey :: Colour
grey = rgb255 211 215 207

-- | Aluminium 3/6 from the Tango icon theme.
darkGrey :: Colour
darkGrey = rgb255 186 189 182

-- | Aluminium 1/6 from the Tango icon theme.
lightGray :: Colour
lightGray = rgb255 238 238 236

-- | Aluminium 2/6 from the Tango icon theme.
gray :: Colour
gray = rgb255 211 215 207

-- | Aluminium 3/6 from the Tango icon theme.
darkGray :: Colour
darkGray = rgb255 186 189 182

-- | Aluminium 4/6 from the Tango icon theme.
lightCharcoal :: Colour
lightCharcoal = rgb255 136 138 133

-- | Aluminium 5/6 from the Tango icon theme.
charcoal :: Colour
charcoal = rgb255 85 87 83

-- | Aluminium 6/6 from the Tango icon theme.
darkCharcoal :: Colour
darkCharcoal = rgb255 46 52 54
