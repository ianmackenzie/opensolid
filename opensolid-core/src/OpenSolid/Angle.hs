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
  , halfPi
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

import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import Prelude qualified

{-|  An angle in degrees, radians, turns etc.

Represented internally as a value in radians.
-}
type Angle = Quantity Radians

-- | The zero value.
zero :: Angle
zero = Quantity.zero

-- | The [golden angle](https://en.wikipedia.org/wiki/Golden_angle).
goldenAngle :: Angle
goldenAngle = radians (Number.pi * (3.0 - Number.sqrt 5.0))

-- | Compute the sine of an angle.
sin :: Angle -> Number
sin (Quantity x) = Quantity (Prelude.sin x)

-- | Compute the cosine of an angle.
cos :: Angle -> Number
cos (Quantity x) = Quantity (Prelude.cos x)

-- | Compute the tangent of an angle.
tan :: Angle -> Number
tan (Quantity x) = Quantity (Prelude.tan x)

-- | Compute the inverse sine of a value.
asin :: Number -> Angle
asin (Quantity x) = Quantity (Prelude.asin x)

-- | Compute the inverse cosine of a value.
acos :: Number -> Angle
acos (Quantity x) = Quantity (Prelude.acos x)

-- | Compute the inverse tangent of a value.
atan :: Number -> Angle
atan (Quantity x) = Quantity (Prelude.atan x)

{-| Compute an angle from Y and X values.

This is the angle from the origin (0,0) to the point (X,Y),
or equivalently the angle of the vector with components (X,Y).

Note the argument order - by convention, Y is given first and X second.
-}
atan2 :: Quantity units -> Quantity units -> Angle
atan2 (Quantity y) (Quantity x) = Quantity (Prelude.atan2 y x)

-- | One radian.
radian :: Angle
radian = radians 1.0

-- | Construct an angle from a number of radians.
radians :: Number -> Angle
radians = Quantity.coerce

-- | Convert an angle to a number of radians.
inRadians :: Angle -> Number
inRadians = Quantity.coerce

-- | π radians, or 180 degrees.
pi :: Angle
pi = radians Number.pi

-- | π/2 radians, or 90 degrees.
halfPi :: Angle
halfPi = radians Number.halfPi

-- | 2π radians, or 360 degrees.
twoPi :: Angle
twoPi = radians Number.twoPi

-- | One degree.
degree :: Angle
degree = fullTurn / 360.0

-- | Construct an angle from a number of degrees.
degrees :: Number -> Angle
degrees = (* degree)

-- | Convert an angle to a number of degrees.
inDegrees :: Angle -> Number
inDegrees = (/ degree)

-- | One full turn, or 360 degrees.
fullTurn :: Angle
fullTurn = twoPi

-- | One half turn, or 180 degrees.
halfTurn :: Angle
halfTurn = pi

-- | One quarter turn, or 90 degrees.
quarterTurn :: Angle
quarterTurn = halfPi

{-| Construct an angle from a number of turns.

One turn is equal to 360 degrees.
-}
turns :: Number -> Angle
turns = (* fullTurn)

{-| Convert an angle to a number of turns.

One turn is equal to 360 degrees.
-}
inTurns :: Angle -> Number
inTurns = (/ fullTurn)
