module OpenSolid.Orientation2D
  ( Orientation2D
  , coerce
  , horizontal
  , vertical
  , fromXDirection
  , fromYDirection
  , rotateLeft
  , rotateRight
  , rotateBy
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
  , random
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2D, Orientation2D (Orientation2D))
import OpenSolid.Random qualified as Random

xDirection :: Orientation2D space -> Direction2D space
xDirection (Orientation2D i _) = i

yDirection :: Orientation2D space -> Direction2D space
yDirection (Orientation2D _ j) = j

coerce :: Orientation2D space1 -> Orientation2D space2
coerce (Orientation2D i j) = Orientation2D (Direction2D.coerce i) (Direction2D.coerce j)

horizontal :: Orientation2D space
horizontal = Orientation2D Direction2D.x Direction2D.y

vertical :: Orientation2D space
vertical = Orientation2D Direction2D.y (negative Direction2D.x)

fromXDirection :: Direction2D space -> Orientation2D space
fromXDirection i = Orientation2D i (Direction2D.rotateLeft i)

fromYDirection :: Direction2D space -> Orientation2D space
fromYDirection j = Orientation2D (Direction2D.rotateRight j) j

rotateLeft :: Orientation2D space -> Orientation2D space
rotateLeft (Orientation2D i j) =
  Orientation2D (Direction2D.rotateLeft i) (Direction2D.rotateLeft j)

rotateRight :: Orientation2D space -> Orientation2D space
rotateRight (Orientation2D i j) =
  Orientation2D (Direction2D.rotateRight i) (Direction2D.rotateRight j)

rotateBy :: Angle -> Orientation2D space -> Orientation2D space
rotateBy angle (Orientation2D i j) =
  Orientation2D (Direction2D.rotateBy angle i) (Direction2D.rotateBy angle j)

placeIn :: Frame2D frameUnits global local -> Orientation2D local -> Orientation2D global
placeIn globalOrientation (Orientation2D i j) =
  Orientation2D
    (Direction2D.placeIn globalOrientation i)
    (Direction2D.placeIn globalOrientation j)

relativeTo :: Frame2D frameUnits global local -> Orientation2D global -> Orientation2D local
relativeTo globalOrientation (Orientation2D i j) =
  Orientation2D
    (Direction2D.relativeTo globalOrientation i)
    (Direction2D.relativeTo globalOrientation j)

-- | Generate a random orientation.
random :: Random.Generator (Orientation2D global)
random = Random.map fromXDirection Direction2D.random
