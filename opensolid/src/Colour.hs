module Colour
  ( Colour
  , rgb
  , fromHexString
  , toHexString
  , black
  , aliceblue
  , antiquewhite
  , aqua
  , aquamarine
  , azure
  , beige
  , bisque
  , blanchedalmond
  , blue
  , blueviolet
  , brown
  , burlywood
  , cadetblue
  , chartreuse
  , chocolate
  , coral
  , cornflowerblue
  , cornsilk
  , crimson
  , cyan
  , darkblue
  , darkcyan
  , darkgoldenrod
  , darkgray
  , darkgreen
  , darkgrey
  , darkkhaki
  , darkmagenta
  , darkolivegreen
  , darkorange
  , darkorchid
  , darkred
  , darksalmon
  , darkseagreen
  , darkslateblue
  , darkslategray
  , darkslategrey
  , darkturquoise
  , darkviolet
  , deeppink
  , deepskyblue
  , dimgray
  , dimgrey
  , dodgerblue
  , firebrick
  , floralwhite
  , forestgreen
  , fuchsia
  , gainsboro
  , ghostwhite
  , gold
  , goldenrod
  , gray
  , grey
  , green
  , greenyellow
  , honeydew
  , hotpink
  , indianred
  , indigo
  , ivory
  , khaki
  , lavender
  , lavenderblush
  , lawngreen
  , lemonchiffon
  , lightblue
  , lightcoral
  , lightcyan
  , lightgoldenrodyellow
  , lightgray
  , lightgreen
  , lightgrey
  , lightpink
  , lightsalmon
  , lightseagreen
  , lightskyblue
  , lightslategray
  , lightslategrey
  , lightsteelblue
  , lightyellow
  , lime
  , limegreen
  , linen
  , magenta
  , maroon
  , mediumaquamarine
  , mediumblue
  , mediumorchid
  , mediumpurple
  , mediumseagreen
  , mediumslateblue
  , mediumspringgreen
  , mediumturquoise
  , mediumvioletred
  , midnightblue
  , mintcream
  , mistyrose
  , moccasin
  , navajowhite
  , navy
  , oldlace
  , olive
  , olivedrab
  , orange
  , orangered
  , orchid
  , palegoldenrod
  , palegreen
  , paleturquoise
  , palevioletred
  , papayawhip
  , peachpuff
  , peru
  , pink
  , plum
  , powderblue
  , purple
  , red
  , rosybrown
  , royalblue
  , saddlebrown
  , salmon
  , sandybrown
  , seagreen
  , seashell
  , sienna
  , silver
  , skyblue
  , slateblue
  , slategray
  , slategrey
  , snow
  , springgreen
  , steelblue
  , tan
  , teal
  , thistle
  , tomato
  , turquoise
  , violet
  , wheat
  , white
  , whitesmoke
  , yellow
  , yellowgreen
  )
where

import Data.Colour qualified
import Data.Colour.Names qualified
import Data.Colour.SRGB qualified
import OpenSolid

type Colour = Data.Colour.Colour Float

rgb :: Float -> Float -> Float -> Colour
rgb = Data.Colour.SRGB.sRGB

fromHexString :: String -> Colour
fromHexString = Data.Colour.SRGB.sRGB24read

toHexString :: Colour -> String
toHexString = Data.Colour.SRGB.sRGB24show

black :: Colour
black = Data.Colour.Names.black

aliceblue :: Colour
aliceblue = Data.Colour.Names.aliceblue

antiquewhite :: Colour
antiquewhite = Data.Colour.Names.antiquewhite

aqua :: Colour
aqua = Data.Colour.Names.aqua

aquamarine :: Colour
aquamarine = Data.Colour.Names.aquamarine

azure :: Colour
azure = Data.Colour.Names.azure

beige :: Colour
beige = Data.Colour.Names.beige

bisque :: Colour
bisque = Data.Colour.Names.bisque

blanchedalmond :: Colour
blanchedalmond = Data.Colour.Names.blanchedalmond

blue :: Colour
blue = Data.Colour.Names.blue

blueviolet :: Colour
blueviolet = Data.Colour.Names.blueviolet

brown :: Colour
brown = Data.Colour.Names.brown

burlywood :: Colour
burlywood = Data.Colour.Names.burlywood

cadetblue :: Colour
cadetblue = Data.Colour.Names.cadetblue

chartreuse :: Colour
chartreuse = Data.Colour.Names.chartreuse

chocolate :: Colour
chocolate = Data.Colour.Names.chocolate

coral :: Colour
coral = Data.Colour.Names.coral

cornflowerblue :: Colour
cornflowerblue = Data.Colour.Names.cornflowerblue

cornsilk :: Colour
cornsilk = Data.Colour.Names.cornsilk

crimson :: Colour
crimson = Data.Colour.Names.crimson

cyan :: Colour
cyan = Data.Colour.Names.cyan

darkblue :: Colour
darkblue = Data.Colour.Names.darkblue

darkcyan :: Colour
darkcyan = Data.Colour.Names.darkcyan

darkgoldenrod :: Colour
darkgoldenrod = Data.Colour.Names.darkgoldenrod

darkgray :: Colour
darkgray = Data.Colour.Names.darkgray

darkgreen :: Colour
darkgreen = Data.Colour.Names.darkgreen

darkgrey :: Colour
darkgrey = Data.Colour.Names.darkgrey

darkkhaki :: Colour
darkkhaki = Data.Colour.Names.darkkhaki

darkmagenta :: Colour
darkmagenta = Data.Colour.Names.darkmagenta

darkolivegreen :: Colour
darkolivegreen = Data.Colour.Names.darkolivegreen

darkorange :: Colour
darkorange = Data.Colour.Names.darkorange

darkorchid :: Colour
darkorchid = Data.Colour.Names.darkorchid

darkred :: Colour
darkred = Data.Colour.Names.darkred

darksalmon :: Colour
darksalmon = Data.Colour.Names.darksalmon

darkseagreen :: Colour
darkseagreen = Data.Colour.Names.darkseagreen

darkslateblue :: Colour
darkslateblue = Data.Colour.Names.darkslateblue

darkslategray :: Colour
darkslategray = Data.Colour.Names.darkslategray

darkslategrey :: Colour
darkslategrey = Data.Colour.Names.darkslategrey

darkturquoise :: Colour
darkturquoise = Data.Colour.Names.darkturquoise

darkviolet :: Colour
darkviolet = Data.Colour.Names.darkviolet

deeppink :: Colour
deeppink = Data.Colour.Names.deeppink

deepskyblue :: Colour
deepskyblue = Data.Colour.Names.deepskyblue

dimgray :: Colour
dimgray = Data.Colour.Names.dimgray

dimgrey :: Colour
dimgrey = Data.Colour.Names.dimgrey

dodgerblue :: Colour
dodgerblue = Data.Colour.Names.dodgerblue

firebrick :: Colour
firebrick = Data.Colour.Names.firebrick

floralwhite :: Colour
floralwhite = Data.Colour.Names.floralwhite

forestgreen :: Colour
forestgreen = Data.Colour.Names.forestgreen

fuchsia :: Colour
fuchsia = Data.Colour.Names.fuchsia

gainsboro :: Colour
gainsboro = Data.Colour.Names.gainsboro

ghostwhite :: Colour
ghostwhite = Data.Colour.Names.ghostwhite

gold :: Colour
gold = Data.Colour.Names.gold

goldenrod :: Colour
goldenrod = Data.Colour.Names.goldenrod

gray :: Colour
gray = Data.Colour.Names.gray

grey :: Colour
grey = Data.Colour.Names.grey

green :: Colour
green = Data.Colour.Names.green

greenyellow :: Colour
greenyellow = Data.Colour.Names.greenyellow

honeydew :: Colour
honeydew = Data.Colour.Names.honeydew

hotpink :: Colour
hotpink = Data.Colour.Names.hotpink

indianred :: Colour
indianred = Data.Colour.Names.indianred

indigo :: Colour
indigo = Data.Colour.Names.indigo

ivory :: Colour
ivory = Data.Colour.Names.ivory

khaki :: Colour
khaki = Data.Colour.Names.khaki

lavender :: Colour
lavender = Data.Colour.Names.lavender

lavenderblush :: Colour
lavenderblush = Data.Colour.Names.lavenderblush

lawngreen :: Colour
lawngreen = Data.Colour.Names.lawngreen

lemonchiffon :: Colour
lemonchiffon = Data.Colour.Names.lemonchiffon

lightblue :: Colour
lightblue = Data.Colour.Names.lightblue

lightcoral :: Colour
lightcoral = Data.Colour.Names.lightcoral

lightcyan :: Colour
lightcyan = Data.Colour.Names.lightcyan

lightgoldenrodyellow :: Colour
lightgoldenrodyellow = Data.Colour.Names.lightgoldenrodyellow

lightgray :: Colour
lightgray = Data.Colour.Names.lightgray

lightgreen :: Colour
lightgreen = Data.Colour.Names.lightgreen

lightgrey :: Colour
lightgrey = Data.Colour.Names.lightgrey

lightpink :: Colour
lightpink = Data.Colour.Names.lightpink

lightsalmon :: Colour
lightsalmon = Data.Colour.Names.lightsalmon

lightseagreen :: Colour
lightseagreen = Data.Colour.Names.lightseagreen

lightskyblue :: Colour
lightskyblue = Data.Colour.Names.lightskyblue

lightslategray :: Colour
lightslategray = Data.Colour.Names.lightslategray

lightslategrey :: Colour
lightslategrey = Data.Colour.Names.lightslategrey

lightsteelblue :: Colour
lightsteelblue = Data.Colour.Names.lightsteelblue

lightyellow :: Colour
lightyellow = Data.Colour.Names.lightyellow

lime :: Colour
lime = Data.Colour.Names.lime

limegreen :: Colour
limegreen = Data.Colour.Names.limegreen

linen :: Colour
linen = Data.Colour.Names.linen

magenta :: Colour
magenta = Data.Colour.Names.magenta

maroon :: Colour
maroon = Data.Colour.Names.maroon

mediumaquamarine :: Colour
mediumaquamarine = Data.Colour.Names.mediumaquamarine

mediumblue :: Colour
mediumblue = Data.Colour.Names.mediumblue

mediumorchid :: Colour
mediumorchid = Data.Colour.Names.mediumorchid

mediumpurple :: Colour
mediumpurple = Data.Colour.Names.mediumpurple

mediumseagreen :: Colour
mediumseagreen = Data.Colour.Names.mediumseagreen

mediumslateblue :: Colour
mediumslateblue = Data.Colour.Names.mediumslateblue

mediumspringgreen :: Colour
mediumspringgreen = Data.Colour.Names.mediumspringgreen

mediumturquoise :: Colour
mediumturquoise = Data.Colour.Names.mediumturquoise

mediumvioletred :: Colour
mediumvioletred = Data.Colour.Names.mediumvioletred

midnightblue :: Colour
midnightblue = Data.Colour.Names.midnightblue

mintcream :: Colour
mintcream = Data.Colour.Names.mintcream

mistyrose :: Colour
mistyrose = Data.Colour.Names.mistyrose

moccasin :: Colour
moccasin = Data.Colour.Names.moccasin

navajowhite :: Colour
navajowhite = Data.Colour.Names.navajowhite

navy :: Colour
navy = Data.Colour.Names.navy

oldlace :: Colour
oldlace = Data.Colour.Names.oldlace

olive :: Colour
olive = Data.Colour.Names.olive

olivedrab :: Colour
olivedrab = Data.Colour.Names.olivedrab

orange :: Colour
orange = Data.Colour.Names.orange

orangered :: Colour
orangered = Data.Colour.Names.orangered

orchid :: Colour
orchid = Data.Colour.Names.orchid

palegoldenrod :: Colour
palegoldenrod = Data.Colour.Names.palegoldenrod

palegreen :: Colour
palegreen = Data.Colour.Names.palegreen

paleturquoise :: Colour
paleturquoise = Data.Colour.Names.paleturquoise

palevioletred :: Colour
palevioletred = Data.Colour.Names.palevioletred

papayawhip :: Colour
papayawhip = Data.Colour.Names.papayawhip

peachpuff :: Colour
peachpuff = Data.Colour.Names.peachpuff

peru :: Colour
peru = Data.Colour.Names.peru

pink :: Colour
pink = Data.Colour.Names.pink

plum :: Colour
plum = Data.Colour.Names.plum

powderblue :: Colour
powderblue = Data.Colour.Names.powderblue

purple :: Colour
purple = Data.Colour.Names.purple

red :: Colour
red = Data.Colour.Names.red

rosybrown :: Colour
rosybrown = Data.Colour.Names.rosybrown

royalblue :: Colour
royalblue = Data.Colour.Names.royalblue

saddlebrown :: Colour
saddlebrown = Data.Colour.Names.saddlebrown

salmon :: Colour
salmon = Data.Colour.Names.salmon

sandybrown :: Colour
sandybrown = Data.Colour.Names.sandybrown

seagreen :: Colour
seagreen = Data.Colour.Names.seagreen

seashell :: Colour
seashell = Data.Colour.Names.seashell

sienna :: Colour
sienna = Data.Colour.Names.sienna

silver :: Colour
silver = Data.Colour.Names.silver

skyblue :: Colour
skyblue = Data.Colour.Names.skyblue

slateblue :: Colour
slateblue = Data.Colour.Names.slateblue

slategray :: Colour
slategray = Data.Colour.Names.slategray

slategrey :: Colour
slategrey = Data.Colour.Names.slategrey

snow :: Colour
snow = Data.Colour.Names.snow

springgreen :: Colour
springgreen = Data.Colour.Names.springgreen

steelblue :: Colour
steelblue = Data.Colour.Names.steelblue

tan :: Colour
tan = Data.Colour.Names.tan

teal :: Colour
teal = Data.Colour.Names.teal

thistle :: Colour
thistle = Data.Colour.Names.thistle

tomato :: Colour
tomato = Data.Colour.Names.tomato

turquoise :: Colour
turquoise = Data.Colour.Names.turquoise

violet :: Colour
violet = Data.Colour.Names.violet

wheat :: Colour
wheat = Data.Colour.Names.wheat

white :: Colour
white = Data.Colour.Names.white

whitesmoke :: Colour
whitesmoke = Data.Colour.Names.whitesmoke

yellow :: Colour
yellow = Data.Colour.Names.yellow

yellowgreen :: Colour
yellowgreen = Data.Colour.Names.yellowgreen
