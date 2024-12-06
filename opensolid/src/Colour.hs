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

type Colour = Data.Colour.Colour Float

rgb :: Float -> Float -> Float -> Colour
rgb = Data.Colour.SRGB.sRGB

rgb255 :: Int -> Int -> Int -> Colour
rgb255 r g b = rgb (r / 255) (g / 255) (b / 255)

hsl :: Angle -> Float -> Float -> Colour
hsl hue saturation lightness =
  Data.Colour.RGBSpace.HSL.hsl (Angle.inDegrees hue) saturation lightness
    |> Data.Colour.RGBSpace.uncurryRGB rgb

fromHex :: Text -> Colour
fromHex = Text.unpack >> Data.Colour.SRGB.sRGB24read

toHex :: Colour -> Text
toHex = Data.Colour.SRGB.sRGB24show >> Text.pack

components :: Colour -> (Float, Float, Float)
components colour = do
  let (Data.Colour.SRGB.RGB r g b) = Data.Colour.SRGB.toSRGB colour
  (r, g, b)

components255 :: Colour -> (Int, Int, Int)
components255 colour = do
  let (r, g, b) = components colour
  (Float.round (r * 255.0), Float.round (g * 255.0), Float.round (b * 255.0))

lightRed :: Colour
lightRed = rgb255 239 41 41

red :: Colour
red = rgb255 204 0 0

darkRed :: Colour
darkRed = rgb255 164 0 0

lightOrange :: Colour
lightOrange = rgb255 252 175 62

orange :: Colour
orange = rgb255 245 121 0

darkOrange :: Colour
darkOrange = rgb255 206 92 0

lightYellow :: Colour
lightYellow = rgb255 255 233 79

yellow :: Colour
yellow = rgb255 237 212 0

darkYellow :: Colour
darkYellow = rgb255 196 160 0

lightGreen :: Colour
lightGreen = rgb255 138 226 52

green :: Colour
green = rgb255 115 210 22

darkGreen :: Colour
darkGreen = rgb255 78 154 6

lightBlue :: Colour
lightBlue = rgb255 114 159 207

blue :: Colour
blue = rgb255 52 101 164

darkBlue :: Colour
darkBlue = rgb255 32 74 135

lightPurple :: Colour
lightPurple = rgb255 173 127 168

purple :: Colour
purple = rgb255 117 80 123

darkPurple :: Colour
darkPurple = rgb255 92 53 102

lightBrown :: Colour
lightBrown = rgb255 233 185 110

brown :: Colour
brown = rgb255 193 125 17

darkBrown :: Colour
darkBrown = rgb255 143 89 2

black :: Colour
black = rgb255 0 0 0

white :: Colour
white = rgb255 255 255 255

lightGrey :: Colour
lightGrey = rgb255 238 238 236

grey :: Colour
grey = rgb255 211 215 207

darkGrey :: Colour
darkGrey = rgb255 186 189 182

lightGray :: Colour
lightGray = rgb255 238 238 236

gray :: Colour
gray = rgb255 211 215 207

darkGray :: Colour
darkGray = rgb255 186 189 182

lightCharcoal :: Colour
lightCharcoal = rgb255 136 138 133

charcoal :: Colour
charcoal = rgb255 85 87 83

darkCharcoal :: Colour
darkCharcoal = rgb255 46 52 54
