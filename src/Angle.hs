module Angle
  ( Angle
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

import Float qualified
import OpenSolid
import Units (Radians)
import Prelude qualified

type Angle = Qty Radians

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
radians (Qty x) = Qty x

inRadians :: Angle -> Float
inRadians (Qty x) = Qty x

degree :: Angle
degree = fullTurn / 360.0

degrees :: Float -> Angle
degrees = (* degree)

inDegrees :: Angle -> Float
inDegrees = (/ degree)

fullTurn :: Angle
fullTurn = radians Float.tau

halfTurn :: Angle
halfTurn = turns 0.5

quarterTurn :: Angle
quarterTurn = turns 0.25

turns :: Float -> Angle
turns = (* fullTurn)

inTurns :: Angle -> Float
inTurns = (/ fullTurn)
