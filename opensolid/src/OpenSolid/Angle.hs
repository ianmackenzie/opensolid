module OpenSolid.Angle
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

import OpenSolid.Arithmetic
import OpenSolid.Float (Float, fromRational)
import OpenSolid.Float qualified as Float
import OpenSolid.Qty (Qty (Qty))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Units (Radians)
import OpenSolid.Units qualified as Units
import Prelude qualified

{-|  An angle in degrees, radians, turns etc.

Represented internally as a value in radians.
-}
type Angle = Qty Radians

-- | The zero value.
zero :: Angle
zero = Qty.zero

-- | The [golden angle](https://en.wikipedia.org/wiki/Golden_angle).
goldenAngle :: Angle
goldenAngle = radians (Float.pi * (3.0 - Float.sqrt 5.0))

-- | Compute the sine of an angle.
sin :: Angle -> Float
sin (Qty x) = Qty (Prelude.sin x)

-- | Compute the cosine of an angle.
cos :: Angle -> Float
cos (Qty x) = Qty (Prelude.cos x)

-- | Compute the tangent of an angle.
tan :: Angle -> Float
tan (Qty x) = Qty (Prelude.tan x)

-- | Compute the inverse sine of a value.
asin :: Float -> Angle
asin (Qty x) = Qty (Prelude.asin x)

-- | Compute the inverse cosine of a value.
acos :: Float -> Angle
acos (Qty x) = Qty (Prelude.acos x)

-- | Compute the inverse tangent of a value.
atan :: Float -> Angle
atan (Qty x) = Qty (Prelude.atan x)

{-| Compute an angle from Y and X values.

This is the angle from the origin (0,0) to the point (X,Y),
or equivalently the angle of the vector with components (X,Y).

Note the argument order - by convention, Y is given first and X second.
-}
atan2 :: Qty units -> Qty units -> Angle
atan2 (Qty y) (Qty x) = Qty (Prelude.atan2 y x)

-- | One radian.
radian :: Angle
radian = radians 1.0

-- | Construct an angle from a number of radians.
radians :: Float -> Angle
radians = Units.coerce

-- | Convert an angle to a number of radians.
inRadians :: Angle -> Float
inRadians = Units.coerce

-- | π radians, or 180 degrees.
pi :: Angle
pi = radians Float.pi

-- | 2π radians, or 360 degrees.
twoPi :: Angle
twoPi = radians Float.twoPi

-- | One degree.
degree :: Angle
degree = fullTurn / 360.0

-- | Construct an angle from a number of degrees.
degrees :: Float -> Angle
degrees = (* degree)

-- | Convert an angle to a number of degrees.
inDegrees :: Angle -> Float
inDegrees = (/ degree)

-- | One full turn, or 360 degrees.
fullTurn :: Angle
fullTurn = radians Float.twoPi

-- | One half turn, or 180 degrees.
halfTurn :: Angle
halfTurn = radians Float.pi

-- | One quarter turn, or 90 degrees.
quarterTurn :: Angle
quarterTurn = radians (0.5 * Float.pi)

{-| Construct an angle from a number of turns.

One turn is equal to 360 degrees.
-}
turns :: Float -> Angle
turns = (* fullTurn)

{-| Convert an angle to a number of turns.

One turn is equal to 360 degrees.
-}
inTurns :: Angle -> Float
inTurns = (/ fullTurn)
