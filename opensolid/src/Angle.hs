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
import Float (Float, fromRational)
import Float qualified
import Qty (Qty (Qty))
import Qty qualified
import Units (Radians)
import Units qualified
import Prelude qualified

{-|  An angle in degrees, radians, turns etc.

Represented internally as a value in radians.
-}
type Angle = Qty Radians

zero :: Angle
zero = Qty.zero

goldenAngle :: Angle
goldenAngle = radians (Float.pi * (3.0 - Float.sqrt 5.0))

sin :: Angle -> Float
sin (Qty x) = Qty (Prelude.sin x)

cos :: Angle -> Float
cos (Qty x) = Qty (Prelude.cos x)

tan :: Angle -> Float
tan (Qty x) = Qty (Prelude.tan x)

asin :: Float -> Angle
asin (Qty x) = Qty (Prelude.asin x)

acos :: Float -> Angle
acos (Qty x) = Qty (Prelude.acos x)

atan :: Float -> Angle
atan (Qty x) = Qty (Prelude.atan x)

atan2 :: Qty units -> Qty units -> Angle
atan2 (Qty y) (Qty x) = Qty (Prelude.atan2 y x)

radian :: Angle
radian = radians 1.0

radians :: Float -> Angle
radians = Units.coerce

inRadians :: Angle -> Float
inRadians = Units.coerce

pi :: Angle
pi = radians Float.pi

twoPi :: Angle
twoPi = radians Float.twoPi

degree :: Angle
degree = fullTurn / 360.0

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
