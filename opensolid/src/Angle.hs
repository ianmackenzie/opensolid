module Angle
  ( Angle
  , zero
  , goldenAngle
  , sin
  , cos
  , tan
  , asin
  , acos
  , atan
  , atan2
  , radians
  , inRadians
  , radian
  , pi
  , twoPi
  , degrees
  , inDegrees
  , degree
  , fullTurn
  , halfTurn
  , quarterTurn
  , turns
  , inTurns
  )
where

import Arithmetic
import Basics
import Float (Float, fromRational)
import Float qualified
import Qty (Qty (Qty, Qty_))
import Qty qualified
import Units (Radians)
import Prelude qualified

type Angle = Qty Radians

zero :: Angle
zero = Qty.zero

goldenAngle :: Angle
goldenAngle = radians (Float.pi * (3.0 - Float.sqrt 5.0))

sin :: Angle -> Float
sin (Qty_ x) = Qty_ (Prelude.sin x)

cos :: Angle -> Float
cos (Qty_ x) = Qty_ (Prelude.cos x)

tan :: Angle -> Float
tan (Qty_ x) = Qty_ (Prelude.tan x)

asin :: Float -> Angle
asin (Qty_ x) = Qty_ (Prelude.asin x)

acos :: Float -> Angle
acos (Qty_ x) = Qty_ (Prelude.acos x)

atan :: Float -> Angle
atan (Qty_ x) = Qty_ (Prelude.atan x)

atan2 :: Qty units -> Qty units -> Angle
atan2 (Qty_ y) (Qty_ x) = Qty_ (Prelude.atan2 y x)

radian :: Angle
radian = radians 1.0

radians :: Float -> Angle
radians = Qty

inRadians :: Angle -> Float
inRadians (Qty x) = x

pi :: Angle
pi = radians Float.pi

twoPi :: Angle
twoPi = radians Float.twoPi

degree :: Angle
degree = fullTurn / 360

degrees :: Float -> Angle
degrees = (* degree)

inDegrees :: Angle -> Float
inDegrees = (/ degree)

fullTurn :: Angle
fullTurn = radians Float.twoPi

halfTurn :: Angle
halfTurn = radians Float.pi

quarterTurn :: Angle
quarterTurn = radians (0.5 * Float.pi)

turns :: Float -> Angle
turns = (* fullTurn)

inTurns :: Angle -> Float
inTurns = (/ fullTurn)
