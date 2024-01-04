module Colour
  ( Colour
  , rgb
  , rgb255
  , fromHexString
  , toHexString
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
import Data.Colour.SRGB qualified
import OpenSolid

type Colour = Data.Colour.Colour Float

rgb :: Float -> Float -> Float -> Colour
rgb = Data.Colour.SRGB.sRGB

rgb255 :: Int -> Int -> Int -> Colour
rgb255 r g b = rgb (r / 255) (g / 255) (b / 255)

fromHexString :: String -> Colour
fromHexString = Data.Colour.SRGB.sRGB24read

toHexString :: Colour -> String
toHexString = Data.Colour.SRGB.sRGB24show

lightRed :: Colour
lightRed = rgb (239.0 / 255.0) (41.0 / 255.0) (41.0 / 255.0)

red :: Colour
red = rgb (204.0 / 255.0) (0.0 / 255.0) (0.0 / 255.0)

darkRed :: Colour
darkRed = rgb (164.0 / 255.0) (0.0 / 255.0) (0.0 / 255.0)

lightOrange :: Colour
lightOrange = rgb (252.0 / 255.0) (175.0 / 255.0) (62.0 / 255.0)

orange :: Colour
orange = rgb (245.0 / 255.0) (121.0 / 255.0) (0.0 / 255.0)

darkOrange :: Colour
darkOrange = rgb (206.0 / 255.0) (92.0 / 255.0) (0.0 / 255.0)

lightYellow :: Colour
lightYellow = rgb (255.0 / 255.0) (233.0 / 255.0) (79.0 / 255.0)

yellow :: Colour
yellow = rgb (237.0 / 255.0) (212.0 / 255.0) (0.0 / 255.0)

darkYellow :: Colour
darkYellow = rgb (196.0 / 255.0) (160.0 / 255.0) (0.0 / 255.0)

lightGreen :: Colour
lightGreen = rgb (138.0 / 255.0) (226.0 / 255.0) (52.0 / 255.0)

green :: Colour
green = rgb (115.0 / 255.0) (210.0 / 255.0) (22.0 / 255.0)

darkGreen :: Colour
darkGreen = rgb (78.0 / 255.0) (154.0 / 255.0) (6.0 / 255.0)

lightBlue :: Colour
lightBlue = rgb (114.0 / 255.0) (159.0 / 255.0) (207.0 / 255.0)

blue :: Colour
blue = rgb (52.0 / 255.0) (101.0 / 255.0) (164.0 / 255.0)

darkBlue :: Colour
darkBlue = rgb (32.0 / 255.0) (74.0 / 255.0) (135.0 / 255.0)

lightPurple :: Colour
lightPurple = rgb (173.0 / 255.0) (127.0 / 255.0) (168.0 / 255.0)

purple :: Colour
purple = rgb (117.0 / 255.0) (80.0 / 255.0) (123.0 / 255.0)

darkPurple :: Colour
darkPurple = rgb (92.0 / 255.0) (53.0 / 255.0) (102.0 / 255.0)

lightBrown :: Colour
lightBrown = rgb (233.0 / 255.0) (185.0 / 255.0) (110.0 / 255.0)

brown :: Colour
brown = rgb (193.0 / 255.0) (125.0 / 255.0) (17.0 / 255.0)

darkBrown :: Colour
darkBrown = rgb (143.0 / 255.0) (89.0 / 255.0) (2.0 / 255.0)

black :: Colour
black = rgb (0.0 / 255.0) (0.0 / 255.0) (0.0 / 255.0)

white :: Colour
white = rgb (255.0 / 255.0) (255.0 / 255.0) (255.0 / 255.0)

lightGrey :: Colour
lightGrey = rgb (238.0 / 255.0) (238.0 / 255.0) (236.0 / 255.0)

grey :: Colour
grey = rgb (211.0 / 255.0) (215.0 / 255.0) (207.0 / 255.0)

darkGrey :: Colour
darkGrey = rgb (186.0 / 255.0) (189.0 / 255.0) (182.0 / 255.0)

lightGray :: Colour
lightGray = rgb (238.0 / 255.0) (238.0 / 255.0) (236.0 / 255.0)

gray :: Colour
gray = rgb (211.0 / 255.0) (215.0 / 255.0) (207.0 / 255.0)

darkGray :: Colour
darkGray = rgb (186.0 / 255.0) (189.0 / 255.0) (182.0 / 255.0)

lightCharcoal :: Colour
lightCharcoal = rgb (136.0 / 255.0) (138.0 / 255.0) (133.0 / 255.0)

charcoal :: Colour
charcoal = rgb (85.0 / 255.0) (87.0 / 255.0) (83.0 / 255.0)

darkCharcoal :: Colour
darkCharcoal = rgb (46.0 / 255.0) (52.0 / 255.0) (54.0 / 255.0)
