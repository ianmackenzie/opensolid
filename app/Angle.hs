module Angle (
    radians,
    inRadians,
    radian,
    degrees,
    inDegrees,
    degree,
) where

import OpenSolid
import qualified Units

radian :: Angle
radian =
    radians 1.0

radians :: Float -> Angle
radians =
    Units.add

inRadians :: Angle -> Float
inRadians =
    Units.drop

degree :: Angle
degree =
    radians (pi / 180)

degrees :: Float -> Angle
degrees =
    (* degree)

inDegrees :: Angle -> Float
inDegrees =
    (/ degree)
